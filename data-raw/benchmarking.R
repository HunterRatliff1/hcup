library(hcup)

get_codes <- function(vers, n=500){
  if(vers=="dx9"){
    codes <- sample(hcup.data::CCS_dx9_map[["I9_DX"]], n)
    return(codes)
  }
  if(vers=="pr9"){
    codes <- sample(hcup.data::CCS_pr9_map[["I9_PR"]], n)
    return(codes)
  }
  if(vers=="dx10"){
    codes <- sample(hcup.data::CCSR_DX_mapping[["I10_DX"]], n)
    return(codes)
  }
  if(vers=="pr10"){
    codes <- sample(hcup.data::CCSR_PR_mapping[["I10_PR"]], n)
    return(codes)
  }
}



#### ccsr_dx ####
bench::mark( ccsr_dx(get_codes("dx10", n=1000)) )

#### classify_ccsr_dx1 ####
bench::mark( classify_ccsr_dx1(get_codes("dx10", n=1000)) )

#### classify_ccs ####
bench::mark( classify_ccs(get_codes("dx9", n=1000),  "dx9", "single")  )
bench::mark( classify_ccs(get_codes("pr9", n=1000),  "pr9", "single")  )
bench::mark( classify_ccs(get_codes("dx10", n=1000), "dx10", "single") )
bench::mark( classify_ccs(get_codes("pr10", n=1000), "pr10", "single") )


#### classify_ccsr_pr ####
bench::mark( classify_ccsr_pr(get_codes("pr10", n=1000)) )


#### classify_chronic ####
bench::mark( classify_chronic(get_codes("dx9", n=1000), "dx9" ) )
bench::mark( classify_chronic(get_codes("dx10", n=1000), "dx10" ) )

#### classify_pr ####
bench::mark( classify_pr(get_codes("pr9", n=1000)) )
bench::mark( classify_pr(get_codes("pr10", n=1000)) )

#### Not checked ####
# lookup_table
# explain_ccs
# explain_ccsr
# reverse_ccsr

