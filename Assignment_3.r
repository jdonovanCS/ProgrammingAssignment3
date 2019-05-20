best <- function(state, outcome_name)
{
    #read in csv data for outcomes
    outcome_of_care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #check if the state is valid
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
    
    #get the best of the column in this dataframe that corresponds to this outcome
    minimum_outcome_in_state <- min(outcomes_in_state[column_name][!is.na(outcomes_in_state[column_name])])
    best_outcome_in_state <- outcomes_in_state[outcomes_in_state[column_name] == minimum_outcome_in_state, ]

    best_outcome_in_state$Hospital.Name
}