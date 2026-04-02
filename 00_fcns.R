#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' SET ID functions
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

# ok now knowing this, create the contact matrix
# which is the probability of interacting with someone in any of these environments
# hmmm. seems like you need to know something about the age structure of each
# environment.
# or maybe its just the fraction of the population that you interact with
# yeah but you need to know the age distribution
# something like
# household = 50% x (person < 25) and 50% x (person >= 25)
# school = 80% (person < 20) and 20% (person > = 20)
# work = 100% (person > 20)
# community = all people
# and then collapsing across using Time activity patterns


get_ids <- function(local_pop_df2, n, column, age_lower, age_higher)  {

  # *****
  # n = grp1_size
  # age_lower = 0
  # age_higher = 24
  # column = 'household_id'
  # ******

  # (1) Get all potential ids
  all_potential_ids <- subset(local_pop_df2, age >= age_lower &
                                age <= age_higher &
                                local_pop_df2[[column]] == 0)$person_id

  # (2) sample from this
  if(n >= length(all_potential_ids)) {

    # probably because there are no more people who are in a specific
    # age bracket?
    cat("Reached the end.\n")

    # set a temporary placeholder which you will over-write later
    rr <- which(local_pop_df2$person_id %in% all_potential_ids)
    local_pop_df2[rr, column] <- -1

    return(list(ids = all_potential_ids,
                local_pop_df = local_pop_df2,
                n = n,
                continue = FALSE))

  } else {

    # get a sample
    this_sample <- sample(all_potential_ids, n, replace = F)

    # set a temporary placeholder which you will over-write later
    rr <- which(local_pop_df2$person_id %in% this_sample)
    local_pop_df2[rr, column] <- -1

    return(list(ids = this_sample,
                local_pop_df = local_pop_df2,
                n = n,
                continue = TRUE))
  }

}

set_ids <- function(local_pop_df, vec, column, p1, p2, age0, age1, age2) {

  # **********
  # local_pop_df = pop_df
  # vec = household_sizes
  # column = "household_id"
  # p1 = 0.5; p2 = 0.5
  # age0 = 0; age1 = 25; age2 = 125
  # **********
  # local_pop_df = pop_df
  # vec = work_sizes
  # column = "work_id"
  # p1 = 0.01; p2 = 0.99
  # age0 = 0;  age1 = 20; age2 = 125
  # ************

  continue = TRUE
  ii = 1
  cat("** Assigning", column, "**:\t")

  while(continue) {

    #
    if(ii %% 1e3 == 0) cat(ii, '\t')

    # this group size
    total_grp_size = vec[ii]

    # grps
    grp1_size = floor(p1 * total_grp_size)
    grp2_size = ceiling(p2 * total_grp_size)

    if(!(grp1_size + grp2_size == total_grp_size)) {
      print(grp1_size)
      print(grp2_size)
      print(total_grp_size)
      stop("error in split math")
    }

    # get IDs for group 1
    if(grp1_size > 0) {
      out1 <- get_ids(local_pop_df, grp1_size, column,
                      age_lower = age0, age_higher = age1 - 1)

      local_continue <- out1$continue
      n1 <- out1$n
      grp1_ids     <- out1$ids
      local_pop_df <- out1$local_pop_df
    } else {
      local_continue = TRUE
      n1 <- 0
      grp1_ids     <- c()
      local_pop_df <- local_pop_df
    }

    # get IDs for group 2
    if(local_continue) {

      out2 <- get_ids(local_pop_df, grp2_size, column,
                      age_lower = age1, age_higher = age2)

      local_continue2 <- out2$continue
      n2 <- out2$n

      if(local_continue2) {

        grp2_ids     <- out2$ids
        local_pop_df <- out2$local_pop_df

        # get all ids
        all_ids <- c(grp1_ids, grp2_ids)
        if(any(is.na(all_ids))) {
          print(all_ids)
          stop('something in all_ids is NA')
        }

        # set
        rr <- which(local_pop_df$person_id %in% all_ids)
        local_pop_df[rr, column] = ii
        if(any(is.na(local_pop_df[rr, get(column)]))) {
          stop("some error in reset math 1")
        }

        if(!(all(local_pop_df[rr, get(column)] >= 0))) {

          rr2 <- which(local_pop_df[rr, get(column)] < 0)
          print(local_pop_df[rr2, ])

          stop("some error in reset math 2")
        }

        ii = ii + 1

        # define the stopping conditions
        if(all(local_pop_df[, get(column)] > 0)) {
          cat("all ids are complete - stopping\n")
          continue = FALSE
        }

      } else {
        cat("out2 continue is FALSE -", total_grp_size, n1, n2,"- \n")
        continue = FALSE
      }

    } else {
      cat("out1 continue is FALSE -",n1,"- \n")
      continue = FALSE
    }
  }

  # set to NA any that are missing
  # this includes any that were 0 and got halfway through
  cat("Number of groups:", ii, "\n")
  cat("Last group size:", vec[ii], "\n")
  # print(head(local_pop_df))
  rr <- which(local_pop_df[, get(column)] <= 0)
  local_pop_df[rr, column] = NA

  return(local_pop_df)
}


set_ids_single <- function(local_pop_df, vec, column, age0, age1) {

  # **********
  # local_pop_df = pop_df
  # vec = household_sizes
  # column = "household_id"
  # p1 = 0.5; p2 = 0.5
  # age0 = 0; age1 = 25; age2 = 125
  # **********
  local_pop_df = pop_df
  vec = community_sizes
  column = "community_id"
  age0 = 0;  age1 = 125
  # ************

  continue = TRUE
  ii = 1
  cat("** Assigning", column, "**:\t")

  while(continue) {

    #
    if(ii %% 1e3 == 0) cat(ii, '\t')

    # this group size
    total_grp_size = vec[ii]

    # grps
    grp1_size = floor(p1 * total_grp_size)
    grp2_size = ceiling(p2 * total_grp_size)

    if(!(grp1_size + grp2_size == total_grp_size)) {
      print(grp1_size)
      print(grp2_size)
      print(total_grp_size)
      stop("error in split math")
    }

    # get IDs for group 1
    if(grp1_size > 0) {
      out1 <- get_ids(local_pop_df, grp1_size, column,
                      age_lower = age0, age_higher = age1 - 1)

      local_continue <- out1$continue
      n1 <- out1$n
      grp1_ids     <- out1$ids
      local_pop_df <- out1$local_pop_df
    } else {
      local_continue = TRUE
      n1 <- 0
      grp1_ids     <- c()
      local_pop_df <- local_pop_df
    }

    # get IDs for group 2
    if(local_continue) {

      out2 <- get_ids(local_pop_df, grp2_size, column,
                      age_lower = age1, age_higher = age2)

      local_continue2 <- out2$continue
      n2 <- out2$n

      if(local_continue2) {

        grp2_ids     <- out2$ids
        local_pop_df <- out2$local_pop_df

        # get all ids
        all_ids <- c(grp1_ids, grp2_ids)
        if(any(is.na(all_ids))) {
          print(all_ids)
          stop('something in all_ids is NA')
        }

        # set
        rr <- which(local_pop_df$person_id %in% all_ids)
        local_pop_df[rr, column] = ii
        if(any(is.na(local_pop_df[rr, get(column)]))) {
          stop("some error in reset math 1")
        }

        if(!(all(local_pop_df[rr, get(column)] >= 0))) {

          rr2 <- which(local_pop_df[rr, get(column)] < 0)
          print(local_pop_df[rr2, ])

          stop("some error in reset math 2")
        }

        ii = ii + 1

        # define the stopping conditions
        if(all(local_pop_df[, get(column)] > 0)) {
          cat("all ids are complete - stopping\n")
          continue = FALSE
        }

      } else {
        cat("out2 continue is FALSE -", total_grp_size, n1, n2,"- \n")
        continue = FALSE
      }

    } else {
      cat("out1 continue is FALSE -",n1,"- \n")
      continue = FALSE
    }
  }

  # set to NA any that are missing
  # this includes any that were 0 and got halfway through
  cat("Number of groups:", ii, "\n")
  cat("Last group size:", vec[ii], "\n")
  # print(head(local_pop_df))
  rr <- which(local_pop_df[, get(column)] <= 0)
  local_pop_df[rr, column] = NA

  return(local_pop_df)
}



plot_dists <- function(x0, x1)  {

  library(ggplot2)

  df1 <- as.data.frame(x1)
  colnames(df1) <- c("size", "freq")
  df1$group <- "pop_df"

  df0 <- as.data.frame(x0)
  colnames(df0) <- c("size", "freq")
  df0$group <- "baseline"

  # Ensure 'size' is numeric
  df1$size <- as.numeric(as.character(df1$size))
  df0$size <- as.numeric(as.character(df0$size))

  df1$prop <- df1$freq / sum(df1$freq)
  df0$prop <- df0$freq / sum(df0$freq)

  df <- rbind(df1, df0)

  ggplot(df, aes(x = size, y = prop, fill = group)) +
    geom_col(position = "dodge") +
    theme_minimal()
}
