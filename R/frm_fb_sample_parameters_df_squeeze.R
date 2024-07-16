## File Name: frm_fb_sample_parameters_df_squeeze.R
## File Version: 0.171


frm_fb_sample_parameters_df_squeeze <- function(coef1, ind_mm, cc, NC)
{
    if (ind_mm$model %in% c('yjtreg','bctreg')){
        if (ind_mm$est_df){
            if (cc==ind_mm$index_df){
                coef1[cc] <- mdmb_squeeze_double(val=coef1[cc],
                                    min=ind_mm$logdf_min,
                                    max=ind_mm$logdf_max)
            }
        }
    }
    return(coef1)
}
