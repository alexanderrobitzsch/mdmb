## File Name: frm_fb_sample_parameters_df_squeeze.R
## File Version: 0.05


frm_fb_sample_parameters_df_squeeze <- function(coef1, ind_mm, cc, NC, M=7)
{
    if (ind_mm$model %in% c("yjtreg","bctreg")){
        if (cc==NC){
            if (ind_mm$R_args$est_df){
                if( coef1[cc] > M ){
                    coef1[cc] <- M
                }
            }
        }
    }
    return(coef1)
}
