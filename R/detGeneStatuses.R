#' Determine gene amplifications and biallelic losses
#'
#' @param out.dir Path to output dir
#' @param input.file.paths A list or vector supplying the path output files from the HMF pipeline.
#' This list should be in the form c(germ_vcf='', som_vcf='', cnv='')
#' @param sample.name Name of the sample
#' @param genes.bed.file Path to the bed file containing the genes of interest. This bed file should 
#' also contain the column ensembl_gene_id
#' @param exons.bed.file Path to the bed file containing the exons from the genes of interest.
#' @param java.path Path the the java binary
#' @param snpsift.path Path to the SnpSift.jar
#' @param snpeff.path Path to snpEff.jar
#' @param do.filter.vcf Filter vcf for PASS variants and select gene regions?
#' @param do.snpeff.ann Annotate SNV/indels variant type with snpeff?
#' calcChromArmPloidies().
#' @param keep.chroms A character vector indicating which chromosomes to keep
#' @param sel.cols.cnv A named character vector
#' @param verbose Show progress messages?
#'
#' @return Writes a diplotypes table to the output dir
#' @export
#'
detGeneStatuses <- function(
   ## Input arguments
   out.dir, input.file.paths=c(germ_vcf=NA, som_vcf=NA, cnv=NA), sample.name,
   
   ## Package constants
   genes.bed.file=GENES_BED_FILE, exons.bed.file=EXONS_BED_FILE,
   java.path=JAVA_PATH, snpsift.path=SNPSIFT_PATH, snpeff.path=SNPEFF_PATH,
   
   ## Defaults for HMF pipeline output
   do.filter.vcf=T, do.snpeff.ann=F,
   keep.chroms=c(1:22,'X'),
   sel.cols.cnv=NULL,
   
   verbose=T
){
   
   options(stringsAsFactors=F)
   
   ## Inputs for debugging ========================================================
   if(F){
      devtools::load_all('/Users/lnguyen/hpc/cuppen/projects/P0013_WGS_patterns_Diagn/CUPs_classifier/processed/cuplr/geneDriverAnnotator/')

      ## HMF
      vcf_paths <- read.delim('/Users/lnguyen/hpc/cuppen/projects/P0013_WGS_patterns_Diagn/datasets/processed/HMF_DR104/metadata/vcf_paths.txt', stringsAsFactors=F)
      #sample.name <- 'CPCT02010422T' ## BRCA2 LOH+som
      #sample.name <- 'CPCT02010543T' ## APC som stop gain (5) + som FS (5)
      #sample.name <- 'CPCT02010349T' ## BRCA2 LOH+som
      #sample.name <- 'CPCT02010794T' ## TERT hotspot
      #sample.name <- 'CPCT02010451T' ## BRAF hotspot
      #sample.name <- 'CPCT02270045T' ## KRAS hotspot
      #sample.name <- 'CPCT02020568T' ## NF2 truncation
      sample.name <- 'CPCT02380011T' ## BRCA1 LOH + false hotspot
      
      #out.parent.dir <- '/Users/lnguyen/hpc/cuppen/projects/P0013_WGS_patterns_Diagn/CUPs_classifier/processed/cuplr/geneDriverAnnotator/test/output/'
      #out.parent.dir <- '/Users/lnguyen/hpc/cuppen/projects/P0013_WGS_patterns_Diagn/datasets/processed/HMF_DR104/gene_ann_2/'
      out.parent.dir <- '/Users/lnguyen/hpc/cuppen/projects/P0013_WGS_patterns_Diagn/datasets/processed/HMF_DR104/scripts/annotate_genes_jesko/raw/'
      out.dir <- paste0(out.parent.dir,'/',sample.name,'/')
      dir.create(out.dir, recursive=T, showWarnings=F)

      input.file.paths <- unlist(vcf_paths[vcf_paths$sample==sample.name,-1])
      input.file.paths <- paste0('/Users/lnguyen/',input.file.paths)
      names(input.file.paths) <- c('germ_vcf','som_vcf','sv_vcf','gene_cnv','cnv')

      ## PCAWG pipeline5
      # out.dir='/Users/lnguyen/hpc/cuppen/projects/P0003_FOOTPRINTS/WGS_clones/processed/Liver_footprint//gene_ann//ALC3_CLONE32/'
      # sample.name=basename(out.dir)
      # input.file.paths=c(
      #    germ_vcf='/Users/lnguyen/hpc/cuppen/projects/P0003_FOOTPRINTS/WGS_clones/processed/Liver_footprint_data/vcf/batch01//c110116R_ALC3CLONE32_ALC3BLOOD/c110116R_ALC3CLONE32_ALC3BLOOD.annotated.vcf.gz',
      #    som_vcf='/Users/lnguyen/hpc/cuppen/projects/P0003_FOOTPRINTS/WGS_clones/processed/Liver_footprint_data/vcf/batch01//c110116R_ALC3CLONE32_ALC3BLOOD//somaticVariants/c110116RALC3BLOOD_c110116RALC3CLONE32/c110116RALC3BLOOD_c110116RALC3CLONE32_post_processed.vcf.gz',
      #    cnv='/Users/lnguyen/hpc/cuppen/projects/P0003_FOOTPRINTS/WGS_clones/processed/Liver_footprint_data/vcf/batch01//c110116R_ALC3CLONE32_ALC3BLOOD//purple/c110116RALC3CLONE32.purple.cnv'
      # )
      
      
      ## PCAWG
      vcf_paths <- read.delim('/Users/lnguyen/hpc/cuppen/projects/P0013_WGS_patterns_Diagn/datasets/processed/PCAWG_2020/manifest/manifest_gene_ann.txt.gz', stringsAsFactors=F)
      #sample.name <- '54195db3-94a9-4538-8bb8-9953d936acd4'
      sample.name <- '1bea3a72-3b73-4072-a6bb-96a90119d3ac'
      
      out.parent.dir <- '/Users/lnguyen/hpc/cuppen/projects/P0013_WGS_patterns_Diagn/datasets/processed/PCAWG_2020/gene_ann/'
      out.dir <- paste0(out.parent.dir,'/',sample.name,'/')
      dir.create(out.dir, recursive=T, showWarnings=F)
      
      input.file.paths <- unlist(vcf_paths[vcf_paths$sample_id==sample.name,-1])
      input.file.paths <- paste0('/Users/lnguyen/',input.file.paths)
      names(input.file.paths) <- c('germ_vcf','som_vcf','cnv')
      
      sel.cols.cnv=c(chrom='chromosome',start='start',end='end',total_cn='total_cn',major_cn='major_cn',minor_cn='minor_cn')
      
      #------
      do.filter.vcf=T
      do.snpeff.ann=F
      keep.chroms=c(1:22,'X')

      ## Common args
      genes.bed.file=GENES_BED_FILE
      #genes.bed.file='/Users/lnguyen/hpc/cuppen/projects/P0013_WGS_patterns_Diagn/datasets/processed/HMF_DR104/scripts/annotate_genes_jesko/genes.txt.gz'
      exons.bed.file=EXONS_BED_FILE
      #exons.bed.file='/Users/lnguyen/hpc/cuppen/projects/P0013_WGS_patterns_Diagn/datasets/processed/HMF_DR104/scripts/annotate_genes_jesko/exons.txt.gz'
      java.path=JAVA_PATH
      snpsift.path='/Users/lnguyen/Documents/R_cache/snpEff/SnpSift.jar'
      snpeff.path='/Users/lnguyen/Documents/R_cache/snpEff/snpEff.jar'
      verbose=T
   }
   
   ## Sanity checks ========================================================
   if(!dir.exists(out.dir)){
      stop('out.dir does not exist: ',out.dir)
   }
   
   input.file.paths <- as.list(input.file.paths)
   if(!all(c('germ_vcf','som_vcf') %in% names(input.file.paths)) & !any(c('cnv','gene_cnv') %in% names(input.file.paths))){
      stop("input.file.paths must have the names: 'germ_vcf','som_vcf'; 'cnv',or 'gene_cnv'")
   }
   
   ## Pre-process somatic/germline vcfs ========================================================
   preproc_dir <- paste0(out.dir,'/preproc/')
   dir.create(preproc_dir, recursive=T, showWarnings=F)
   
   preproc_files <- list()
   
   bed_file <- read.delim(genes.bed.file, stringsAsFactors=F, check.names=F)
   #print(genes.bed.file)
   #print(exons.bed.file)
   
   preprocSnvIndelVcfs <- function(mut_type=c('germ','som')){
      # mut_type='som'
      
      MUT_VCF <- paste0(mut_type,'_vcf')
      MUT_TXT <- paste0(mut_type,'_txt')
      
      preproc_files[[MUT_VCF]] <<- paste0(preproc_dir,'/',sample.name,'.',mut_type,'.vcf.gz')
      preproc_files[[MUT_TXT]] <<- paste0(preproc_dir,'/',sample.name,'.',mut_type,'.txt.gz')
      
      if(verbose){ message('\n## Processing ',toupper(mut_type),' vcf...') }
      
      if(  !file.exists(preproc_files[[MUT_VCF]]) ){
         
         if(do.filter.vcf){
            if(verbose){ message('> Filtering vcf on variants in genes in bed file and PASS variants...') }
            filterVcf(
               vcf.file = input.file.paths[[MUT_VCF]],
               out.file = preproc_files[[MUT_VCF]],
               mode = mut_type,
               bed.file = genes.bed.file,
               java.path = java.path,
               snpsift.path = snpsift.path
            )
         } else {
            message('> Skipping filtering vcf; copying vcf to out dir...')
            system(paste(
               'cp',
               input.file.paths[[MUT_VCF]],
               preproc_files[[MUT_VCF]]
            ))
         }
         
      } else {
         if(verbose){ message('> Skipping filtering vcf; output file exists: ',preproc_files[[MUT_VCF]]) }
      }
      
      ## Snpeff annotation
      if(do.snpeff.ann){
         
         ann_done <- paste0(preproc_dir,'/ann.',mut_type,'.done')
         
         if(!file.exists(ann_done)){
            if(verbose){ message('> Annotating variants...') }
            annotateVariantType(
               vcf.file = preproc_files[[MUT_VCF]], 
               out.file = preproc_files[[MUT_VCF]],
               genome = 'GRCh37.75',
               java.path = java.path, 
               snpeff.path = snpeff.path
            )
            
            file.create(ann_done)
         } else {
            if(verbose){ message("> Skipping annotating variants; done file exists") }
         }
      }
      
      ## Filtering
      if( !file.exists(preproc_files[[MUT_TXT]]) ){
         if(verbose){ message('> Extracting revelant fields in vcf to txt...') }
         extractVcfFields(
            vcf.file = preproc_files[[MUT_VCF]],
            out.file = preproc_files[[MUT_TXT]],
            java.path = java.path,
            snpsift.path = snpsift.path
         )
         
         txt <- read.delim(preproc_files[[MUT_TXT]], stringsAsFactors=F)
         if(nrow(txt)!=0){ 
            txt$chrom <- as.character(txt$chrom)
            GenomeInfoDb::seqlevelsStyle(txt$chrom)<- 'NCBI' 
         }
         GenomeInfoDb::seqlevelsStyle(keep.chroms)<- 'NCBI'
         
         if(!is.null(keep.chroms)){
            if(verbose){ message('> Keeping chromosomes: ',paste(keep.chroms, collapse=',')) }
            txt <- txt[txt$chrom %in% keep.chroms,]
         }
         
         if(verbose){ message('> Subsetting for ENSG ids present in bed file...') }
         txt <- txt[txt$ensembl_gene_id %in% bed_file$ensembl_gene_id,]
         
         if(verbose){ message('> Adding HGNC gene ids...') }
         #txt$hgnc_symbol <- ensgToHgncSymbol(txt$ensembl_gene_id)
         txt$hgnc_symbol <- bed_file[match(txt$ensembl_gene_id, bed_file$ensembl_gene_id),'hgnc_symbol']
         
         if(nrow(txt)!=0){
            if(verbose){ message('> Adding ClinVar annotations...') }
            txt$clinvar_sig <- getClinSig(txt, CLINVAR_PATH) ## seqminer::tabix.read returns error if dataframe is empty
            txt$is_hotspot_mut <- detIsHotspotMut(txt, HOTSPOTS_PATH) 
            
            #txt$clinvar_sig[ txt$is_hotspot_mut ] <- 'Pathogenic'
            
         } else {
            if(verbose){ message('> No variants remain after filtering. Skipping adding ClinVar annotations...') }
            txt <- cbind(txt, data.frame(clinvar_sig=character(), is_hotspot_mut=logical()))
         }
         
         write.tsv(txt, preproc_files[[MUT_TXT]])
         rm(txt)
         
      } else {
         if(verbose){ message('> Skipping making txt; output file exists: ',preproc_files[[MUT_TXT]]) }
      }
   }
   
   preprocSnvIndelVcfs('som')
   
   if(!is.na(input.file.paths$germ_vcf)){
      preprocSnvIndelVcfs('germ')
   } else {
      if(verbose){ message('\n## Skipping germline vcf processing as none was provided') }
   }
   
   
   ## Make preliminary output ========================================================
   mut_profile_dir <- paste0(out.dir,'/mut_profiles/')
   dir.create(mut_profile_dir, recursive=T, showWarnings=F)
   
   mut_profile_paths <- list(
      gene_cnv=paste0(mut_profile_dir,'/mut_profile_gene_cnv.txt.gz'),
      som=paste0(mut_profile_dir,'/mut_profile_som.txt.gz'),
      germ=paste0(mut_profile_dir,'/mut_profile_germ.txt.gz')
   )
   
   if(is.na(input.file.paths$germ_vcf)){ 
      mut_profile_paths$germ <- NULL
   }
   
   if( all(file.exists(unlist(mut_profile_paths))) ){
      mut_profile <- lapply(mut_profile_paths, function(i){
         tryCatch({
            read.delim(i, stringsAsFactors=F)
         }, error=function(error_condition){
            data.frame()
         })
      })
   } else {
      mut_profile <- list()
      
      if(verbose){ message('\n## Annotating gene CNV table...') }
      mut_profile$gene_cnv <- mkMutProfileGeneCnv(
         cnv.file=input.file.paths$cnv,
         #gene.cnv.file=input.file.paths$gene_cnv,
         sel.cols=sel.cols.cnv,
         exons.bed.file=exons.bed.file,
         genes.bed.file=genes.bed.file,
         verbose=verbose
      )
      write.tsv(mut_profile$gene_cnv,mut_profile_paths$gene_cnv)
      
      if(verbose){ message('\n## Annotating germline and somatic mutations...') }
      if(verbose){ message('> som...') }
      som_txt <- read.delim(preproc_files[['som_txt']], stringsAsFactors=F)
      mut_profile$som <- mkMutProfileSnvIndel(
         som_txt,
         scoring=SCORING_MUT,
         keep.only.first.eff=T,
         filter.no.impact.variants=F,
         verbose=verbose
      )
      write.tsv(mut_profile$som,mut_profile_paths$som)
      
      if(!is.na(input.file.paths$germ_vcf)){
         if(verbose){ message('> germ...') }
         germ_txt <- read.delim(preproc_files[['germ_txt']], stringsAsFactors=F)
         mut_profile$germ <- mkMutProfileSnvIndel(
            germ_txt,
            scoring=SCORING_MUT,
            keep.only.first.eff=T,
            filter.no.impact.variants=F,
            verbose=verbose
         )
         write.tsv(mut_profile$germ,mut_profile_paths$germ)
      }
   }
   
   ## Force factors to characters
   mut_profile <- lapply(mut_profile, function(i){
      #i=mut_profile[[1]]
      as.data.frame(lapply(i, function(j){
         if(is.factor(j)){ as.character(j) }
         else(j)
      }), stringsAsFactors=F)
   })
   
   ## Make gene diplotypes ========================================================
   if(verbose){ message('\n## Making gene diplotypes tables...') }
   l_gene_diplotypes <- list()
   
   if(verbose){ message('> cnv_som...') }
   ## cnv_som placed first so that somatic mutations are prioritized over germline mutations
   l_gene_diplotypes$cnv_som <- mkGeneDiplotypesCnvMut(
      mut.profile.cnv = mut_profile$gene_cnv, 
      mut.profile.mut = mut_profile$som,
      mut.origin = 'som',
      verbose = verbose
   ) 
   
   if(!is.na(input.file.paths$germ_vcf)){
      if(verbose){ message('> cnv_germ...') }
      l_gene_diplotypes$cnv_germ <- mkGeneDiplotypesCnvMut(
         mut.profile.cnv = mut_profile$gene_cnv, 
         mut.profile.mut = mut_profile$germ, 
         mut.origin = 'germ',
         verbose=verbose
      )
   }

   if(verbose){ message('> som_som...') }
   l_gene_diplotypes$som_som <- mkGeneDiplotypesMutMut(
      mut.profile.mut1 = mut_profile$som, 
      mut.profile.mut2 = mut_profile$som, 
      diplotype.origin = 'som_som',
      verbose = verbose
   )
   
   if(!is.na(input.file.paths$germ_vcf)){
      if(verbose){ message('> germ_som...') }
      l_gene_diplotypes$germ_som <- mkGeneDiplotypesMutMut(
         mut.profile.mut1 = mut_profile$germ, 
         mut.profile.mut2 = mut_profile$som, 
         diplotype.origin = 'germ_som',
         verbose=verbose
      )
   }

   # subset(l_gene_diplotypes$cnv_som, hgnc_symbol %in% c('BRCA1','BRCA2'))
   # View(subset(l_gene_diplotypes$cnv_germ, hgnc_symbol %in% c('BRCA1','BRCA2')))
   # lapply(l_gene_diplotypes, function(i){ which(apply(i,1,anyNA)) })
   
   ## Calculate hit scores and determine gene max effect ========================================================
   if(verbose){ message('\n## Merging diplotype origins into one table...') }
   gene_diplotypes <- do.call(rbind, l_gene_diplotypes)
   rownames(gene_diplotypes) <- NULL
   
   ## Fix to remove duplicate rows for deep_deletion (due to being reported in both cnv_germ and cnv_som)
   gene_diplotypes <- unique(gene_diplotypes)
   
   if(verbose){ message('## Calculating hit_scores...') }
   gene_diplotypes <- insColAfter(
      gene_diplotypes,
      gene_diplotypes$a1.max_score + gene_diplotypes$a2.max_score,
      colname='hit_score',
      after='hgnc_symbol'
   )
   
   if(verbose){ message('## Determining biallelic hit type...') }
   gene_diplotypes <- insColAfter(
      gene_diplotypes,
      data.frame(
         biall_status=detBiallStatus(gene_diplotypes),
         biall_type=detBiallType(gene_diplotypes)
      ),
      after='diplotype_origin'
   )
   
   if(verbose){ message('## Determining most pathogenic diplotype per gene...') }
   gene_diplotypes_max <- getGeneDiplotypeMaxEff(gene_diplotypes)
   ##subset(gene_diplotypes_max, hit_score>=9)
   
   amp_summary <- (function(){
      df <- mut_profile$gene_cnv[
         match(gene_diplotypes_max$ensembl_gene_id, mut_profile$gene_cnv$ensembl_gene_id),
         c('gain_type','gain_ratio_genome','gain_ratio_arm','gain_ratio_focal','gain_ratio_max')
      ]
      df$gain_type <- factor(df$gain_type, c('none','chrom','arm','focal'))
      return(df)
   })()
   
   gene_diplotypes_max <- insColAfter(
      gene_diplotypes_max,
      amp_summary,
      after='hgnc_symbol'
   )
   
   ## Export output tables ========================================================
   if(verbose){ message('## Exporting gene diplotype tables...') }
   #write.tsv(gene_diplotypes,paste0(out.dir,'/gene_diplotypes_full.txt.gz'))
   write.tsv(gene_diplotypes_max,paste0(out.dir,'/gene_diplotypes.txt.gz'))
   
}











