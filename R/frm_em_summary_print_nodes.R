## File Name: frm_em_summary_print_nodes.R
## File Version: 0.10

frm_em_summary_print_nodes <- function( dv_vars, nodes, nodes_description,
            desc_vars )
{
    nodes_string <- paste0( " * Variable ", dv_vars, ": " )
    # frequency of missing values for dependent variable
    dv_fmiss <- desc_vars[ desc_vars$variable==dv_vars, "N_miss"]
    if ( dv_fmiss==0 ){
        nodes_string <- paste0( nodes_string, "No missing values." )
    } else {
        nodes_string <- paste0( nodes_string, length(nodes), " ",
                            nodes_description, " from ",
                            round(min(nodes),2), " to ", round(max(nodes),2) )
    }
    return(nodes_string)
}
