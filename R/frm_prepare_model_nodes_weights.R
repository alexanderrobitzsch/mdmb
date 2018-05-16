## File Name: frm_prepare_model_nodes_weights.R
## File Version: 0.23

frm_prepare_model_nodes_weights <- function( ind_mm , dat0, nodes_control )
{
    dv_vars <- ind_mm$dv_vars    
    ind_mm$nodes_description <- "prespecified nodes"
        
    #--------- descriptives
    y <- dat0[, dv_vars ]
    y <- y[ ! is.na(y) ]    
    if ( length(y) > 0 ){
        m0 <- mean( y , na.rm=TRUE )
        sd0 <- stats::sd( y , na.rm=TRUE )
    } else {
        m0 <- 0
        sd0 <- 1
    }
        
    #--------- define nodes if not provided
    choose_nodes <- sum( names(ind_mm) == "nodes" ) == 0    
    if ( choose_nodes ){
        #*** linear regression
        if ( ind_mm$model %in% c("linreg") ){
            # nodes_low <- min( m0 - nodes_control[2] * sd0 , min(y) )
            # nodes_upp <- max( m0 + nodes_control[2] * sd0 , max(y) )
            nodes_low <- m0 - nodes_control[2] * sd0
            nodes_upp <- m0 + nodes_control[2] * sd0
            nodes_mm <- seq( nodes_low , nodes_upp , length = nodes_control[1] )
            ind_mm$nodes_description <- "automatically chosen nodes"                            
            ind_mm$nodes <- nodes_mm
        }        
        #*** bct regression
        if ( ind_mm$model %in% c("bctreg") ){
            n_nodes <- nodes_control[1]
            probs <- c( 1 / (2*n_nodes) , ( 2*n_nodes - 1 ) / (2*n_nodes) )
            nodes <- stats::quantile( y , probs = probs )
            nodes_mm <- seq( nodes[1] , nodes[2] , len = n_nodes )        
            ind_mm$nodes_description <- "automatically chosen nodes"                            
            ind_mm$nodes <- nodes_mm
        }                        
        #*** logistic regression
        if ( ind_mm$model %in% c("logistic") ){
            ind_mm$nodes <- c(0,1)
            ind_mm$nodes_description <- "nodes consisting of observed values"
        }
        #*** ordinal probit regression
        if ( ind_mm$model %in% c("oprobit") ){        
            nodes <- sort( unique( na.omit(y) ) )
            ind_mm$nodes <- nodes
            ind_mm$nodes_description <- "nodes consisting of observed values"
        }
    }
    
    #--------- define initial weights for nodes
    if ( is.null( ind_mm$nodes_weights) ){
        nodes_mm <- ind_mm$nodes
        NM <- length(nodes_mm)
        #*** linear regression
        if ( ind_mm$model %in% c("linreg","yjtreg") ){
            nodes_weights <- stats::dnorm( nodes_mm , mean=m0, sd=sd0)
        }
        #*** logistic regression
        if ( ind_mm$model %in% c("logistic") ){
            node_freq <- rep(0,NM)
            for (hh in 1:NM){
                node_freq[hh] <- sum( y == nodes_mm[hh] , na.rm=TRUE )
            }
            nodes_weights <- node_freq
        }
        #*** bct regression
        if ( ind_mm$model %in% c("bctreg") ){
            nodes_weights <- stats::dnorm( nodes_mm , mean=m0 , sd = sd0)
        }    
        #*** ordinal probit regression
        if ( ind_mm$model %in% c("oprobit") ){
            nodes_weights <- prop.table( table(y) )
        }    
        
        #---- normalize nodes
        nodes_weights <- frm_normalize_vector(vec=nodes_weights)
        ind_mm$nodes_weights <- nodes_weights
    }
    
    return(ind_mm)
}
