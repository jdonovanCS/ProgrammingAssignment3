rankall <- function(outcome_name, num = "best") {
    ## Read outcome data
    #read in csv data for outcomes
    outcome_of_care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

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
    # outcomes_in_state <- outcome_of_care[outcome_of_care$State == state, ]
    outcome_of_care <- na.omit(outcome_of_care, cols=c(column_name))
    outcome_of_care <- outcome_of_care[outcome_of_care[column_name] != 'Not Available', ]
    outcome_of_care[column_name] <- lapply(outcome_of_care[column_name], function(x) if(is.integer(as.integer(x))) as.numeric(x) else x)
    # outcomes_in_state <- transform(outcomes_in_state, column_name = as.numeric(column_name))
    outcome_of_care <- outcome_of_care[order(outcome_of_care[column_name], outcome_of_care['Hospital.Name']), ]

    #recode num if needed
    if (num == 'best') num <- 1
    if (num == 'worst') {
        num <- 1
        outcome_of_care <- outcome_of_care[order(outcome_of_care[column_name], outcome_of_care['Hospital.Name'], decreasing=TRUE), ]
    }
    if (num > length(outcome_of_care[,1])) return(NA)

    ## For each state, find the hospital of the given rank
    state_specific_dataframe_list <- split(outcome_of_care, outcome_of_care$State)
    rankall_df <- data.frame()
    for (s_df in state_specific_dataframe_list)
    {
        if ((length(rankall_df) < 1) & (is.na(s_df[num, 'State'])))
        {
            rankall_df <- c(s_df[1,'State'], s_df[num,column_name])
        }
        else if (length(rankall_df) < 1)
        {
            rankall_df <- s_df[num,]
        }
        else if (is.na(s_df[num, column_name]) | s_df[1, 'State'] == '<NA>')
        {
            rankall_df <- rbind(rankall_df, c(s_df[1, 'State'], s_df[num, column_name]))
        }
        else
        {
            rankall_df <- rbind(rankall_df, s_df[num,])
        }
    }

    ## Return a data frame with the hospital names and the
    rankall_df <- rankall_df[, c('State', 'Hospital.Name')]
    rankall_df
    ## (abbreviated) state name
}