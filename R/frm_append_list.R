## File Name: frm_append_list.R
## File Version: 0.12

frm_append_list <- function(list1, list2, overwrite=TRUE)
{    
    list2_names <- names(list2)
    if ( ! overwrite ){
        list1_names <- names(list1)
        list2_names <- setdiff(list2_names, list1_names)    
    }
    N2 <- length(list2_names)
    if (N2>0){
        for (nn in 1:N2){
            list1[[ list2_names[nn] ]] <- list2[[ list2_names[nn] ]]
        }
    }
    return(list1)
}
