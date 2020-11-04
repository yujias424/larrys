#‘ Fetch the TCGA Transcriptome Profiling data
#' 
#' This function is used to fetch the TCGA data using TCGABiolink package
#' 
#' @param tcga_type TCGA cancer type to download
#' @param download_location location of download file
#' @param data_type default "Gene Expression Quantification"
#' @param workflow_type "HTSeq - FPKM-UQ"
#' @param sample_type "Primary Tumor"
#' @return GDCprepare objects that can be further used for get exp matrix
#' @export
#' 
fetch_tcga_data <- function(tcga_type, 
                            download_location, 
                            data_type = "Gene Expression Quantification",
                            workflow_type = "HTSeq - FPKM-UQ",
                            sample_type = "Primary Tumor"){
  g_query_tumor <- TCGAbiolinks::GDCquery(project = paste0("TCGA-", tcga_type),
                            data.category = "Transcriptome Profiling",
                            legacy = F,
                            data.type = data_type,
                            workflow.type = workflow_type,
                            sample.type = sample_type)
  TCGAbiolinks::GDCdownload(g_query_tumor, 
                            directory = paste0(download_location, "/", tcga_type, "_tumor"))
  tumor <- TCGAbiolinks::GDCprepare(g_query_tumor, save = FALSE, 
                      directory = paste0(download_location, "/", tcga_type, "_tumor"))
  return(tumor)
}

