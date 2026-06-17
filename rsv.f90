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
! Subroutine for set_ids
!--------------------------------------------------------------------------
subroutine set_ids(df, nrows, ncols,  age_col, vec, zero_col, &
                    & p1, p2, age0, age1, age2)

    !# **********
    !# local_pop_df = pop_df
    !# vec = household_sizes
    !# column = "household_id"
    !# p1 = 0.5; p2 = 0.5
    !# age0 = 0; age1 = 25; age2 = 125
    !***********************

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
    real(kind=8)              :: df(nrows, ncols) ! a real matrix
    integer, intent(in)       :: vec(nrows) ! a vector of group sizes
    real(kind=8), intent(in)  :: p1, p2   ! probabilities of the age groups
    real(kind=8), intent(in)  :: age0, age1, age2 ! age group cutoffs

    ! helpers
    integer                   :: xcontinue, not_all_assigned
    integer                   :: ii, k
    integer                   :: total_grp_size, grp1_size, grp2_size

    ! blanks to pass to get_ids
    integer                   :: rr(nrows)  ! which rows to be selected
    real(kind=8)              :: ids(nrows) ! an array of ids, passed in a reset to 0
    integer                   :: ids_size   ! how many to count forward in ids
    integer                   :: n1, n2     ! how many in each group

    ! initialize
    ii = 1
    xcontinue = 1

    ! also start the df you are writing to
    ! open (unit=20,file='df.txt',action="write", status="replace")

    ! =========================================================================
    ! /////////////////////////////////////////////////////////////////////////
    ! START THE LOOP
    ! since its a while continue, just use line number statements
    ! /////////////////////////////////////////////////////////////////////////
    ! =========================================================================

40  if(mod(ii, 1000) .eq. 0) then
      write(*,*) ii
    end if

    ! set this group size
    total_grp_size = vec(ii)

    ! get group sizes
    grp1_size = floor(p1 * float(total_grp_size))
    grp2_size = ceiling(p2 * float(total_grp_size))

    ! check split math
    if((total_grp_size) .NE. (grp1_size + grp2_size)) then
      write(*,*) 'error in split math'
      return
    end if

    ! initialize
    ! need to define rr, ids, and ids_size
    rr(:)     = 0
    ids_size  = 0
    ids(:)    = 0
    n1        = INT(0)
    xcontinue = 1

    ! get ids for group 1
    if(grp1_size .gt. 0) then


      call get_ids(df, nrows, ncols, age_col, age0, age1, zero_col, rr, &
                 & grp1_size, xcontinue, ids, ids_size)

      n1 = ids_size
      ! write(*,*) "g1 xcontinue", xcontinue

    end if

    ! now continue for the 2nd group
    ! initialize again
    rr(:)     = 0
    ids_size  = 0
    ids(:)    = 0

    ! xcontinue 1
    if(xcontinue .eq. 1) then

      call get_ids(df, nrows, ncols, age_col, age1, age2 + 1, zero_col, rr, &
           & grp2_size, xcontinue, ids, ids_size)

      n2 = ids_size
      ! write(*,*) "g2 xcontinue", xcontinue

      ! xcontinue 2
      if(xcontinue .eq. 1) then

        ! **** (1) ESSENTIALLY HERE I WANT TO EXPORT THE DF

        ! essentially its the ones that are -1
        do k = 1, nrows
          if (df(k, zero_col) .eq. -1.0) then
             df(k, zero_col) = float(ii)
             !write(20,*) df(k, :)
             !df(k, zero_col) = -1.0
          end if
        end do

        ! **** (2) and reduce the size of df and nrows
        ! so create a tmp, allocate it, delete df and then
        ! rename it
        ! and then

        ! iterate
        ii = ii + 1

        ! define the stopping conditions
        not_all_assigned = 0
        do k = 1, nrows
          if (df(k, zero_col) .le. 0.0) then
             not_all_assigned = 1
          end if
        end do

        if(not_all_assigned .eq. 0) then
          write(*,*) "all ids are complete - stopping"
          xcontinue = 0
        end if

      else

        write(*,*) "out2 continue is FALSE -", total_grp_size, n1, n2
        xcontinue = 0

      end if ! 2

    else

      write(*,*) "out1 continue is FALSE -",n1
      xcontinue = 0

    end if ! 1

    if (xcontinue .eq. 1) goto 40


    ! =========================================================================
    ! /////////////////////////////////////////////////////////////////////////
    ! OUTPUT
    ! /////////////////////////////////////////////////////////////////////////
    ! =========================================================================
    close(20)

    write(*,*) "Number of groups:", ii
    write(*,*) "Last group size:", vec(ii)
    do k = 1, nrows
      if (df(k, zero_col) .le. 0.0) then
         df(k, zero_col) = -999.99
      end if
    end do

end subroutine set_ids

!--------------------------------------------------------------------------
! Subroutine for set_ids
!--------------------------------------------------------------------------
subroutine set_ids_single(df, nrows, ncols,  age_col, vec, zero_col, &
                    & age0, age1)

    !# **********
    !# local_pop_df = pop_df
    !# vec = household_sizes
    !# column = "household_id"
    !# p1 = 0.5; p2 = 0.5
    !# age0 = 0; age1 = 25; age2 = 125
    !***********************

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
    real(kind=8)              :: df(nrows, ncols) ! a real matrix
    integer, intent(in)       :: vec(nrows) ! a vector of group sizes
    real(kind=8), intent(in)  :: age0, age1 ! age group cutoffs

    ! helpers
    integer                   :: xcontinue, not_all_assigned
    integer                   :: ii, k
    integer                   :: total_grp_size, grp1_size

    ! blanks to pass to get_ids
    integer                   :: rr(nrows)  ! which rows to be selected
    real(kind=8)              :: ids(nrows) ! an array of ids, passed in a reset to 0
    integer                   :: ids_size   ! how many to count forward in ids
    integer                   :: n1     ! how many in each group

    ! initialize
    ii = 1
    xcontinue = 1

    ! also start the df you are writing to
    ! open (unit=20,file='df.txt',action="write", status="replace")

    ! =========================================================================
    ! /////////////////////////////////////////////////////////////////////////
    ! START THE LOOP
    ! since its a while continue, just use line number statements
    ! /////////////////////////////////////////////////////////////////////////
    ! =========================================================================

40  if(mod(ii, 1000) .eq. 0) then
      write(*,*) ii
    end if

    write(*,*) ii

    ! set this group size
    total_grp_size = vec(ii)

    ! get group sizes
    grp1_size = total_grp_size

    ! initialize
    ! need to define rr, ids, and ids_size
    rr(:)     = 0
    ids_size  = 0
    ids(:)    = 0
    n1        = INT(0)
    xcontinue = 1

    ! get ids for group 1
    call get_ids(df, nrows, ncols, age_col, age0, age1, zero_col, rr, &
               & grp1_size, xcontinue, ids, ids_size)

    write(*,*) ids

    n1 = ids_size

    ! now continue for the 2nd group
    ! initialize again
    rr(:)     = 0
    ids_size  = 0
    ids(:)    = 0

    ! xcontinue 1
    if(xcontinue .eq. 1) then

        ! essentially its the ones that are -1
        do k = 1, nrows
          if (df(k, zero_col) .eq. -1.0) then
             df(k, zero_col) = float(ii)
             !write(20,*) df(k, :)
             !df(k, zero_col) = -1.0
          end if
        end do

        ! **** (2) and reduce the size of df and nrows
        ! so create a tmp, allocate it, delete df and then
        ! rename it
        ! and then

        ! iterate
        ii = ii + 1

        ! define the stopping conditions
        not_all_assigned = 0
        do k = 1, nrows
          if (df(k, zero_col) .le. 0.0) then
             not_all_assigned = 1
          end if
        end do

        if(not_all_assigned .eq. 0) then
          write(*,*) "all ids are complete - stopping"
          xcontinue = 0
        end if

    else

      write(*,*) "out1 continue is FALSE -",n1
      xcontinue = 0

    end if ! 1

    if (xcontinue .eq. 1) goto 40


    ! =========================================================================
    ! /////////////////////////////////////////////////////////////////////////
    ! OUTPUT
    ! /////////////////////////////////////////////////////////////////////////
    ! =========================================================================
    close(20)

    write(*,*) "Number of groups:", ii
    write(*,*) "Last group size:", vec(ii)
    do k = 1, nrows
      if (df(k, zero_col) .le. 0.0) then
         df(k, zero_col) = -999.99
      end if
    end do

end subroutine set_ids_single


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
    if (n > sum(rr)) then
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
    ! it has to return an integer
    rr(:) = INT(0)

    ! iterate
    ! GREATER THAN OR EQUAL TO LOWER BOUND but
    ! JUST LESS THAN UPPER BOUND
    do i = 1, nrows
      if(df(i, age_col) .ge. age_lb .and. df(i, age_col) .lt. age_ub &
      & .and. df(i, zero_col) == 0) then
        rr(i) = INT(1)
      end if
    end do

end subroutine fsubset
