## File Name: frm_fb_verbose_mh_refresh.R
## File Version: 0.05

frm_fb_verbose_mh_refresh <- function( verbose, iter )
{
    if (verbose){
        h1 <- ' - Update proposal standard deviations in MH algorithm'
        h1 <- paste0( h1, ' (Iteration ', iter, ')\n')
        cat(h1)
    }
}
