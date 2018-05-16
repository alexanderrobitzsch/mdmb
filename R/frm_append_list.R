## File Name: frm_append_list.R
## File Version: 0.04

frm_append_list <- function(list1, list2)
{
    N2 <- length(list2)
    if (N2>0){
        for (nn in 1:N2){
            list1[[ names(list2)[nn] ]] <- list2[[nn]]
        }
    }
    return(list1)
}
