!--------------------------------------------------------------------------
! Author: CWM
! Date started: 2.27.2026
! Purpose: Subroutines for creating populations
!
! NOTES:
! - All subroutines need to be in this file
! - For doing in parallel, you may need to add THREADPRIVATE somewhere
! https://fortran-lang.org/learn/best_practices/allocatable_arrays/
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
! Subroutine for get_ids
!--------------------------------------------------------------------------
subroutine get_ids(df, nrows, ncols, age_col, age_lb, age_ub, zero_col, rr, &
                 & n, xcontinue, ids, ids_size)

    implicit none

    ! =========================================================================
    ! /////////////////////////////////////////////////////////////////////////
    ! VARIABLE DEFINTIONS
    ! /////////////////////////////////////////////////////////////////////////
    ! =========================================================================

    integer, intent(in)       :: nrows     ! the number of rows of df
    integer, intent(in)       :: ncols     ! the number of columns of df
    integer, intent(in)       :: age_col   ! which columns is the age_column
    integer, intent(in)       :: zero_col  ! which column is currently being sorted on
    integer, intent(in)       :: n         ! the size of the group to get, decreases over time
    real(kind=8), intent(in)  :: age_lb    ! the age lower bound
    real(kind=8), intent(in)  :: age_ub    ! the age upper bound
    real(kind=8)              :: df(nrows, ncols) ! a real matrix
    integer                   :: xcontinue  ! a boolean flag to say if you've reached the end
    integer                   :: rr(nrows)  ! output row ids
    real(kind=8)              :: ids(nrows) ! an array of ids, passed in a reset to 0
    integer                   :: ids_size   ! how many of the first ids should be used?

    ! helpers
    integer :: i, j

    ! ROWS can be an integers
    integer, allocatable      :: all_potential_rows(:)
    integer                   :: this_sample(n)

    ! IDS are real, these are the first column of df
    real(kind=8), allocatable :: all_potential_ids(:)

    ! Finally reset ids to 0
    ids(:) = 0.0

    ! =========================================================================
    ! /////////////////////////////////////////////////////////////////////////
    ! GET THE SUBSET
    ! /////////////////////////////////////////////////////////////////////////
    ! =========================================================================

    ! # (1) Get all potential ids
    !  all_potential_ids <- subset(local_pop_df2, age >= age_lower &
    !                            age <= age_higher &
    !                            local_pop_df2[[column]] == 0)$person_id
    ! ok so first you get the subset of dataframe where the folloing conditions
    ! are met:
    ! * age is within the lower and upper bounds defined
    ! * and the current column has not yet been allocated to a group
    ! the return of this subroutin is rr, the rows that are in the subset
    call fsubset(df, nrows, ncols, age_col, age_lb, age_ub, zero_col, rr)

    ! Now that you have the rows (rr = 1)
    ! get the person_ids that go along with this
    ! so first allocate
    allocate(all_potential_rows(sum(rr)))
    allocate(all_potential_ids(sum(rr)))
    j = 0
    do i = 1, nrows
      if(rr(i) > 0) then
        j = j + 1
        all_potential_rows(j) = i        ! the row id
        all_potential_ids(j)  = df(i, 1) ! the person id, always the first column
      end if
    end do

    ! =========================================================================
    ! /////////////////////////////////////////////////////////////////////////
    ! OUTPUT
    ! Now one of two things happens:
    ! (1) either the subset you've grabbed is smaller than the desired subset
    !     which has size n --> which means you've finished
    !  or
    ! (2) you have a full grab to pass back
    ! /////////////////////////////////////////////////////////////////////////
    ! =========================================================================

    ! *****************
    ! now check if you've reached the end
    ! probably because there are no more people in this age bracket
    if (n >= sum(rr)) then
    ! *****************

      ! set a temporary placeholder in zero_col that you will deal with later
      do j = 1, sum(rr)
        df(all_potential_rows(j), zero_col) = -1
      end do

      ! and then update id_size, ids, df, n, and continue:
      ids_size = sum(rr)
      do i = 1, ids_size
        ids(i) = all_potential_ids(i)
      end do
      ! df <passed back as is>
      ! n <passed back as is>
      ! continue
      xcontinue = 0

    ! *****************
    else
    ! *****************

      ! } else {

      ! # get a sample
      ! this_sample <- sample(all_potential_ids, n, replace = F)
      ! switching to rows because thats what you use in the next step
      ! x = all_potential_rows
      ! subx = this_sample
      ! n = size of x
      ! k = size of subx
      call ransam(all_potential_rows, this_sample, sum(rr), n)

      ! # set a temporary placeholder which you will over-write later
      ! rr <- which(local_pop_df2$person_id %in% this_sample)
      ! local_pop_df2[rr, column] <- -1
      do j = 1, n
        df(this_sample(j), zero_col) = -1
      end do

      ! update :
      ids_size = n
      do i = 1, ids_size
        ids(i) = this_sample(i)
      end do
      ! df <passed back as is>
      ! n <passed back as is>
      ! continue
      xcontinue = 1

    end if

    !write(*,*) "** IDs **"
    !write(*,*) ids

end subroutine get_ids

!--------------------------------------------------------------------------
! Subroutine for random sampling without replacement
! from 1977: https://link.springer.com/content/pdf/10.3758/BF03214009.pdf
!   x = all_potential_rows
!   subx = this_sample
!   n = size of x
!   k = size of subx
!--------------------------------------------------------------------------
subroutine ransam (x, subx, n, k)

    implicit none

    !
    integer, intent(in) :: n, k
    integer        :: j, m
    real(kind = 8) :: L
    real(kind = 8) :: r

    integer :: x(N)
    integer :: subx(K)

    m = INT(0)

  do j = 1, n

      !
      call random_number(r)
      L = INT ((FLOAT (n - j + 1)) * r) + 1

      !
      if (L .GT. (k - m)) GO TO 50

      ! Now update A
      m = m + 1
      subx(m) = x(j)

      ! if m >= k then go to
      if (m .GE. k) GO TO 99

50  end do

99  return

end subroutine ransam

!--------------------------------------------------------------------------
! Subroutine for subsetting
!--------------------------------------------------------------------------
subroutine fsubset(df, nrows, ncols, age_col, age_lb, age_ub, zero_col, rr)

    ! *******************
    ! INPUTS:
    ! df       - the data frame, defined by nrows and ncols
    ! age_col  - which columns defines the AGE
    ! age_lb   - lower bound of age to select
    ! age_ub   - upper bound of age to select
    ! zero_col - also checking that a different column is zero_co
    ! rr       - the output rows
    ! *******************

    implicit none

    ! INPUTS
    integer, intent(in)       :: nrows, ncols, age_col, zero_col
    real(kind=8), intent(in)  :: age_lb, age_ub
    real(kind=8), intent(in)  :: df(nrows, ncols)

    ! helpers
    integer :: i

    ! Output row ids
    integer :: rr(nrows)

    ! Initialize
    rr(:) = INT(0)

    ! iterate
    do i = 1, nrows
      if(df(i, age_col) >= age_lb .and. df(i, age_col) < age_ub &
      & .and. df(i, zero_col) == 0) then
        rr(i) = INT(1)
      end if
    end do

end subroutine fsubset
