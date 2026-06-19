clean_loans <- function(df){
    df_clean <- df %>% select(loan_status, loan_amnt, term, int_rate, grade, emp_length, home_ownership, annual_inc,
                              verification_status, addr_state, dti, pub_rec, revol_util, purpose,
                              earliest_cr_line) %>%
        # Create outcome variable with cases being 1 = defaulted, 0 = fully paid, NA = unresolved
        mutate(Default_Flag = ifelse(loan_status == "Fully Paid", 0,
                                     ifelse(loan_status %in% c("Charged Off", "Default"), 1, NA_real_))) %>%
        # Strip text from term and emp_length, leaving just the number
        mutate(term = as.numeric(gsub(" months", "", term)), emp_length_num = as.numeric(gsub("[^0-9]", "", emp_length))) %>%
        # Grade has a natural order (A = safest, G = riskiest)  so set set it explicitly
        mutate(grade = factor(grade, levels = c("A","B","C","D","E","F","G")))
    # Winsorise continuous variables used in analysis (caps extremes at 1st/99th percentile)
    df_clean <- Winsorise_Loans(df_clean, col = "dti", BottomQ = 0.01, TopQ = 0.99)
    df_clean <- Winsorise_Loans(df_clean, col = "annual_inc", BottomQ = 0.01, TopQ = 0.99)
    df_clean
}