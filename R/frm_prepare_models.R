## File Name: frm_prepare_models.R
## File Version: 0.545

frm_prepare_models <- function(dep, ind, dat0, nodes_control, nodes_weights=TRUE,
        use_grad=2, use_gibbs=FALSE, weights=NULL )
{
    all_vars <- NULL
    #*** independent variables models
    NM <- length(ind)
    dv_vars <- NULL
    no_weights <- is.null(weights)

    for (mm in 1L:NM){
        res <- frm_formula_extract_terms( ind[[mm]]$formula )
        all_vars <- c( all_vars, res$all_vars )
        dv_vars <- c( dv_vars, res$dv_vars )
        names_ind <- names(ind)
        names_ind[mm] <- paste(res$dv_form)
        names(ind) <- names_ind
        res <- frm_append_list( list1=ind[[mm]], list2=res )
        ## depends on regression model classes
        res1 <- frm_define_model_R_function(model=res, use_grad=use_grad,
                    use_gibbs=use_gibbs, R_args=ind[[mm]]$R_args,
                    sampling_level=ind[[mm]]$sampling_level,
                    variable_level=ind[[mm]]$variable_level, dat0=dat0 )
        res1$no_weights <- no_weights
        ind[[mm]] <- frm_append_list( list1=res, list2=res1 )
        if (nodes_weights){
            ind[[mm]] <- frm_prepare_model_nodes_weights( ind_mm=ind[[mm]],
                                dat0=dat0, nodes_control=nodes_control)
                                ## depends on regression model classes
        }
    }
    attr(ind,'NM') <- NM

    #*** dependent variable model
    res <- frm_formula_extract_terms( dep$formula )
    all_vars <- unique( c( all_vars, res$all_vars ) )
    res <- frm_append_list( list1=dep, list2=res )
    res1 <- frm_define_model_R_function(model=res, use_grad=use_grad,
                    use_gibbs=use_gibbs, R_args=dep$R_args,
                    sampling_level=dep$sampling_level, variable_level=dep$variable_level,
                    dat0=dat0)
    res1$no_weights <- no_weights
    dep <- frm_append_list( list1=res, list2=res1 )
    if (nodes_weights){
        dep <- frm_prepare_model_nodes_weights( ind_mm=dep, dat0=dat0,
                    nodes_control=nodes_control)
    }

    #*** create matrix of variables
    dv <- dep$dv_vars
    iv <- setdiff(all_vars, dv)
    vars <- unique( c( dv, dv_vars, iv ) )
    NV <- length(vars)
    predictorMatrix <- matrix(0, nrow=NV, ncol=NV)
    rownames(predictorMatrix) <- colnames(predictorMatrix) <- vars
    predictorMatrix[ dep$dv_vars, dep$iv_vars ] <- 1
    for (mm in 1L:NM){
        inp <- ind[[mm]]
        predictorMatrix[ inp$dv_vars, inp$iv_vars ] <- 1
    }
    rsp <- rowSums(predictorMatrix)
    var_order <- order( rsp, decreasing=TRUE )
    predictorMatrix <- predictorMatrix[ var_order, var_order ]

    #- check predictor matrix
    frm_check_predictor_matrix(pred=predictorMatrix)

    variablesMatrix <- predictorMatrix
    rvm <- rownames(variablesMatrix)
    variablesMatrix[ cbind( rvm, rvm ) ] <- 1

    #*** output
    res <- list( dep=dep, ind=ind, predictorMatrix=predictorMatrix,
            variablesMatrix=variablesMatrix )
    return(res)
}
