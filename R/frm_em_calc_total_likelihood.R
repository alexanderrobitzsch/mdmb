## File Name: frm_em_calc_total_likelihood.R
## File Version: 0.123

frm_em_calc_total_likelihood <- function(dat, weights0,
    like_obs, post_miss, like=NULL)
{
    eps <- 1E-50

    # calculate total likelihood per case
    # like_case <- like_obs * post_miss
    # like_case <- rowsum( like_case, dat$case )[,1]

    #--- original computation
    # like_case <- like_obs * post_miss
    # like_case <- rowsum( like_case, dat$case )[,1]
    # ll <- sum( log( like_case + eps ) * weights0 )

    if (is.null(like)){
        like_case <- like_obs * post_miss
        like_case <- rowsum( like_case, dat$case )[,1]
        ll <- sum( log( like_case + eps ) * weights0 )
    } else {
        like_case <- like * dat$delta_nodes
        like_case <- rowsum( like_case, dat$case )[,1]
        ll <- sum( log( like_case + eps ) * weights0 )
    }
    return(ll)
}
