## File Name: frm_check_predictor_matrix.R
## File Version: 0.05

frm_check_predictor_matrix <- function(pred)
{
    vars <- rownames(pred)
    #  extract dependent variables
    pred_sum <- rowSums(pred)
    iv <- vars[pred_sum==0]
    dv <- vars[pred_sum>0]
    pred1 <- pred[dv, dv, drop=FALSE]
    pred_check <- pred1 + t(pred1)
    if ( any(pred_check>1) ){
        NC <- nrow(pred_check)
        check1 <- ( pred_check > 1 ) | ( t(pred_check) > 1 )
        cat("No ordered sequence of variable has been defined.\n")
        cat("Problems involve following pairs of variables:\n")
        for (hh in 1:NC){
            for (ii in hh:NC){
                if (check1[hh,ii]){
                    cat( "  ", paste0(dv[hh], " - ", dv[ii] ), "\n")
                }            
            }
        }
        # stop() # stop algorithm here
    }
}
