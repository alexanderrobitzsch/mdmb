## File Name: frm_fb_verbose_iterations.R
## File Version: 0.121

frm_fb_verbose_iterations <- function( verbose, iter, print_iter, maxiter,
        mcmc_start_time )
{
    if ( iter %% print_iter==0 ){
        if (verbose){
            s_temp <- Sys.time()
            t0 <- difftime( s_temp, mcmc_start_time )
            time_diff <- ( maxiter - iter ) * t0 / iter
            units <- 'mins'
            time_diff <- as.numeric( time_diff, units=units )
            time_diff <- round( time_diff, digits=2 )
            t0 <- round( as.numeric( t0, units=units ), digits=2 )
            # p1 <- paste0('*** Iteration ', iter, ' | ', s_temp, ' | ',
            #         'Remaining time ', time_diff, ' ', units, ' \n' )
            p1 <- paste0('*** Iteration ', iter, ' | ',
                     'Remaining: ', time_diff, ' ', units,
                    ' | Elapsed: ', t0, ' ', units,
                    ' \n' )
            cat(p1)
            utils::flush.console()
        }
    }
}
