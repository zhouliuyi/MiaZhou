alphabetical <- function(...) !is.unsorted(c(...))

tcga_aliquotFilter = function(tsb, analyte_target=c("DNA","RNA"), decreasing=TRUE, analyte_position=20, plate=c(22,25)){
    analyte_target = match.arg(analyte_target)
    # Strings in R are largely lexicographic
    # see ??base::Comparison
    
    # find repeated samples
    sampleID = substr(tsb, start = 1, stop = 15)
    dp_samples = sampleID[duplicated(sampleID)]
    
    if(length(dp_samples)==0){
        message("ooo Not find any duplicated barcodes, return original input..")
        tsb
    }else{
        uniq_tsb = tsb[! sampleID %in% dp_samples]
        dp_tsb = setdiff(tsb, uniq_tsb)
        
        add_tsb = c()
    
        # analyte = substr(dp_tsb, start = analyte_position, stop = analyte_position)
        # if analyte_target = "DNA"
        # analyte:  D > G,W,X
        if(analyte_target == "DNA"){
            for(x in dp_samples){
                mulaliquots = dp_tsb[substr(dp_tsb,1,15) == x]
                analytes = substr(mulaliquots, 
                                  start = analyte_position,
                                  stop = analyte_position)
                if(any(analytes == "D")){
                    aliquot = mulaliquots[which(analytes == "D")]
                    add_tsb = c(add_tsb, aliquot)
                    dp_tsb = setdiff(dp_tsb, mulaliquots)
                }
            
            }
        }
        # if analyte_target = "RNA"
        # analyte: H > R > T 
        # else{
        #     
        # }
        if(length(dp_tsb) == 0){
            message("ooo Filter barcodes successfully!")
            c(uniq_tsb, add_tsb)
        }else{
            sampleID_res = substr(dp_tsb, start=1, stop=15)
            dp_samples_res = sampleID_res[duplicated(sampleID_res)]
            
            for(x in dp_samples_res){
                mulaliquots = dp_tsb[substr(dp_tsb,1,15) == x]
                plate_codes = substr(mulaliquots,
                                    start = plate[1],
                                    stop = plate[2])
                plate_keep = sort(plate_codes, decreasing = decreasing)[1]
                add_tsb = c(add_tsb, plate_keep)
                dp_tsb = setdiff(dp_tsb, plate_codes)
            }
            
            if(length(dp_tsb)==0){
                message("ooo Filter barcodes successfully!")
                c(uniq_tsb, add_tsb)
            }else{
                message("ooo barcodes ", dp_tsb, " failed in filter process, other barcodes will be returned.")
                c(uniq_tsb, add_tsb)
            }
        }
    }
    
}
    
    