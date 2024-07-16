## File Name: plot.frm_fb.R
## File Version: 0.103

plot.frm_fb <- function(x, idparm=NULL, ask=TRUE, ... )
{
    parms_mcmc <- x$parms_mcmc
    tech_summary <- x$tech_summary
    partable <- x$partable
    ind0 <- x$ind0
    if ( is.null(idparm) ){
        idparm <- tech_summary$idparm
    }
    x1 <- parms_mcmc$iter_save
    NI <- length(idparm)
    for (ii in 1L:NI){
        ts_ii <- tech_summary[ tech_summary$idparm==idparm[ii], ]
        y1 <- parms_mcmc$values[, idparm[ii] ]
        pm_ii <- partable[tech_summary$idparm==idparm[ii], ]
        name_ii <- ts_ii$parm
        ind0_mm <- ind0[[ as.numeric(ts_ii$model) ]]
        graphics::par(ask=ask)
        title_ii <- paste0( ind0_mm$model, ' | ', name_ii,
                        ' | M=', round( pm_ii$est, 3 ),
                        ' | SE=', round( pm_ii$se, 3 ),
                        '\n',
                        'ESS=', round(ts_ii[,'effsize'],1),
                        ' | Rhat=', round(ts_ii[,'Rhat'],2),
                        ' | Acc. Rate=', round(ts_ii[,'accrate'],2)
                                )
        graphics::plot( x=x1, y=y1, main=title_ii, xlab='Iteration', ylab='Values',
                        type='l')
    }
    graphics::par(ask=ask)
}
