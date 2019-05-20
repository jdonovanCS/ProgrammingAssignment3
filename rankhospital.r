rankhospital <- function(state, outcome_name, num = "best") {

    #read in csv data for outcomes
    outcome_of_care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    if (!state %in% outcome_of_care$State)
    {
        return ('The state is invalid')
    }

    #check if the outcome is valid
    if (!tolower(outcome_name) %in% c('heart attack', 'heart failure', 'pneumonia'))
    {
        return ('The outcome is invalid')
    }
    
    #recode the outcome to match the column name
    column_name <- 'Hospital.30.Day.Death..Mortality..Rates.from.'
    if (tolower(outcome_name) == 'heart attack') column_name <- paste(column_name, 'Heart.Attack', sep='')
    if (tolower(outcome_name) == 'heart failure') column_name <- paste(column_name, 'Heart.Failure', sep='')
    if (tolower(outcome_name) == 'pneumonia') column_name <- paste(column_name, 'Pneumonia', sep='')

    #get the rows in the dataframe that correspond to this state
    outcomes_in_state <- outcome_of_care[outcome_of_care$State == state, ]
    outcomes_in_state <- na.omit(outcomes_in_state, cols=c(column_name))
    outcomes_in_state <- outcomes_in_state[outcomes_in_state[column_name] != 'Not Available', ]
    outcomes_in_state[column_name] <- lapply(outcomes_in_state[column_name], function(x) if(is.integer(as.integer(x))) as.numeric(x) else x)
    # outcomes_in_state <- transform(outcomes_in_state, column_name = as.numeric(column_name))
    outcomes_in_state <- outcomes_in_state[order(outcomes_in_state[column_name], outcomes_in_state['Hospital.Name']), ]

    #recode num if needed
    if (num == 'best') num <- 1
    if (num == 'worst') num <- length(outcomes_in_state[,1])

    #get the best of the column in this dataframe that corresponds to this outcome
    specific_rank_outcome <- outcomes_in_state[num,]
    # outcomes_in_state_vector <- outcomes_in_state[column_name]
    # outcomes_in_state_vector <- outcomes_in_state_vector[!is.na(outcomes_in_state_vector)]
    # outcomes_in_state_vector <- outcomes_in_state_vector['Not Available' != outcomes_in_state_vector]
    # outcomes_in_state_vector <- as.numeric(outcomes_in_state_vector)

    # #recode num if needed
    # if (num == 'best') num <- 1
    # if (num == 'worst') num <- length(outcomes_in_state_vector)

    # #get specific value of outcome at that rank
    # specific_rank_value_for_outcome <- sort(outcomes_in_state_vector)[num]

    # #get hospital name for outcome with this value
    # best_outcome_in_state <- outcomes_in_state[outcomes_in_state[column_name] == specific_rank_value_for_outcome, ]

    ## Return hospital name in that state with the given rank
    # sort(best_outcome_in_state$Hospital.Name)[1]

    specific_rank_outcome$Hospital.Name
}