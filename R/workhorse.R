
#--------------------------------------------------------------------------
# HELPERS
#--------------------------------------------------------------------------


# takes out the Mode of a vector. In case of ties it takes the first value
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Mode(c(1,1,0,0,1,0))

# optional printer
print_o <- function(deb, object) {
  if (deb == T) {
    print(object)
  }
}

# optional messenger
message_o <- function(deb, ...) {
  if (deb == T) {
    message(...)
  }
}


#--------------------------------------------------------------------------
# CDR_CODER
#--------------------------------------------------------------------------

# takes a data frame with only id-variable and glo-variables. Returns it with
# a CDR variable. Option deb = T/F controls printing of intermediate steps.

cdr_coder <- function(df_pure, deb = F) {

  for(i in 1:nrow(df_pure)) {

    # -- process individual

    message_o(deb = deb, "--------------------------------------- iteration ",i)

    # take out two vectors with memory and secondary categories
    vec_sec <- as.numeric(df_pure[i, c("glo6", "glo7", "glo8", "glo9", "glo10"),
                                  drop = F])
    vec_mem <- as.numeric(df_pure[i, "glo5", drop = F])

    message_o(deb = deb, "vec_mem")
    print_o(deb = deb, vec_mem)
    message_o(deb = deb, "vec_sec")
    print_o(deb = deb, vec_sec)

    # -- create intermediate variables

    # secondary categories with the same value as mem
    sec_cat_same_as_mem <- vec_sec == vec_mem
    sum_sec_cat_same_as_mem <- sum(sec_cat_same_as_mem)

    message_o(deb = deb, "sum same as mem")
    print_o(deb = deb, sum_sec_cat_same_as_mem)

    # secondary categories with a greater value than memo
    sec_cat_greater_than_mem <- vec_sec > vec_mem
    sum_sec_cat_greater_than_mem <- sum(sec_cat_greater_than_mem)

    message_o(deb = deb, "sum greater than mem")
    print_o(deb = deb, sum_sec_cat_greater_than_mem)

    # secondary categories with a lesser value than mem
    sec_cat_lesser_than_mem <- vec_sec < vec_mem
    sum_sec_cat_lesser_than_mem <- sum(sec_cat_lesser_than_mem)

    message_o(deb = deb, "sum lesser than mem")
    print_o(deb = deb, sum_sec_cat_lesser_than_mem)

    # total sum, secondary categories with value different than mem
    totsum_sec_cat_different_from_mem <- sum_sec_cat_greater_than_mem +
    sum_sec_cat_lesser_than_mem

    message_o(deb = deb, "totsum different from mem")
    print_o(deb = deb, totsum_sec_cat_different_from_mem)

    # which side of mem is the largest
    side_of_mem_greatest <- ifelse(
      sum_sec_cat_greater_than_mem >
        sum_sec_cat_lesser_than_mem,
      "right",
      ifelse(
        sum_sec_cat_greater_than_mem ==
          sum_sec_cat_lesser_than_mem,
        "tie",
        "left"
      )
    )

    print_o(deb = deb, side_of_mem_greatest)


    # two secondary categories one side, three on the other
    if (totsum_sec_cat_different_from_mem != 5) {
      three_cats_two_cats <- 0
    } else {
      three_cats_two_cats <- ifelse(
        (sum_sec_cat_greater_than_mem %in% c(2, 3)) &
          (sum_sec_cat_lesser_than_mem %in% c(2, 3)) == T,
        1,
        0
      )
    }

    message_o(deb = deb, "twothreecats")
    print_o(deb = deb, three_cats_two_cats)

    # take out left and right vectors
    vec_left <- vec_sec[sec_cat_lesser_than_mem]

    message_o(deb = deb, "vec_left")
    print_o(deb = deb, vec_left)

    vec_right <- vec_sec[sec_cat_greater_than_mem]

    message_o(deb = deb, "vec_right")
    print_o(deb = deb, vec_right)

    # sum secondary categories with value >= 1
    sum_sec_cat_greater_than_one <- sum(vec_sec >= 1)

    # sum secondary categories with value >=0.5
    sum_sec_cat_impairment <- sum(vec_sec >= 0.5)

    # -- Assign a diagnosis

    # standard value given to each person in order to track diagnosis through rules
    CDR <- 100

    message_o(deb = deb, "diagnosis - initial")
    print_o(deb = deb, CDR)

    # MAIN RULES

    # three or more secondary cats with the same value as mem
    if (sum_sec_cat_same_as_mem >= 3) {
      CDR <- vec_mem
    }

    message_o(deb = deb, "diagnosis - main rule 1")
    print_o(deb = deb, CDR)

    # three or more secondary cats with value != mem
    if (totsum_sec_cat_different_from_mem >= 3) {
      # if right
      if (side_of_mem_greatest == "right") {
        CDR <- Mode(vec_right)
        # if left
      } else if (side_of_mem_greatest == "left") {
        CDR <- Mode(vec_left)
        # else tie
      } else {
        CDR <- 200
      }
    }

    message_o(deb = deb, "diagnosis - main rule 2")
    print_o(deb = deb, CDR)

    # three secondary cats on one side of mem, two on the other
    if (three_cats_two_cats == 1) {
      CDR <- vec_mem
    }

    message_o(deb = deb, "diagnosis - main rule 3")
    print_o(deb = deb, CDR)

    # SECONDARY RULES

    # if mem = 0.5 then CDR = 1 if at least three sec cats >= 1
    if (vec_mem == 0.5 & sum_sec_cat_greater_than_one >= 3) {
      CDR <- 1
    }

    message_o(deb = deb, "diagnosis - secondary rule 1")
    print_o(deb = deb, CDR)


    # if mem = 0.5 then CDR cannot be 0, only 0.5 or 1
    if (vec_mem == 0.5 & CDR == 0) {
      CDR <- 0.5
    }

    message_o(deb = deb, "diagnosis - secondary rule 2")
    print_o(deb = deb, CDR)

    # if mem = 0 CDR -> 0 unless there is impairment in at least two sec cats
    if (vec_mem == 0) {
      CDR <- 0
    }

    message_o(deb = deb, "diagnosis - secondary rule 3-1")
    print_o(deb = deb, CDR)

    if (vec_mem == 0 & sum_sec_cat_impairment >= 2) {
      CDR <- 0.5
    }

    message_o(deb = deb, "diagnosis - secondary rule 3-2")
    print_o(deb = deb, CDR)

    # SPECIAL RULES

    # (1) ties one one side of mem, that is m + 1 = 3, the others 2 2 1 1 etc.
    if (sum_sec_cat_lesser_than_mem == 4 & length(unique(vec_left)) == 2) {
      if (sum(vec_left %in% unique(vec_left)[1]) == 2) {
        CDR <- max(unique(vec_left))
      }
    }

    message_o(deb = deb, "diagnosis - special rule 1-1")
    print_o(deb = deb, CDR)

    if (sum_sec_cat_greater_than_mem == 4 & length(unique(vec_right)) == 2) {
      if (sum(vec_right %in% unique(vec_right)[1]) == 2) {
        CDR <- min(unique(vec_right))
      }
    }

    message_o(deb = deb, "diagnosis - special rule 1-2")
    print_o(deb = deb, CDR)

    # (2) only 1 or 2 sec cats equal memory
    if (sum_sec_cat_same_as_mem == 1 | sum_sec_cat_same_as_mem == 2) {
      if (sum_sec_cat_lesser_than_mem <= 2 & sum_sec_cat_greater_than_mem <= 2) {
        CDR <- vec_mem
      }
    }

    message_o(deb = deb, "diagnosis - special rule 2")
    print_o(deb = deb, CDR)

    # (3) if memory >=1 CDR cannot be 0
    if (vec_mem >= 1 & CDR == 0 & Mode(vec_sec) == 0) {
      CDR <- 0.5
    }

    message_o(deb = deb, "diagnosis - special rule 3")
    print_o(deb = deb, CDR)

    # write CDR to original data frame and return
    df_pure[i,"CDRGLOBAL"] <- CDR
  }
  return(df_pure)
}
