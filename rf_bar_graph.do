//do "/snfs2/HOME/wgodwin/chn_practicum_code2/risk_factors/rf_bar_graph.do"
// prep stata
    clear all
    set more off
    set maxvar 32000
    pause on

    local username = c(username)
    if c(os) == "Unix" {
        global prefix "/home/j"
        global prefixh "/homes/`username'"
        set odbcmgr unixodbc
    }
    else if c(os) == "Windows" {
        global prefix "J:"
        global prefixh "H:"
    }

    local code_dir 
    do "$prefix/Usable/Tools/ADO/pdfmaker_Acrobat11.do"
    run "$prefix/temp/central_comp/libraries/current/stata/get_outputs.ado"
    run "$prefix/temp/central_comp/libraries/current/stata/get_cause_metadata.ado"
    run "$prefix/temp/central_comp/libraries/current/stata/get_rei_metadata.ado"
    run "$prefix/temp/central_comp/libraries/current/stata/get_location_metadata.ado"

//locations metadata    
    get_location_metadata, location_set_id(9) clear
    keep location_id location_name
    tempfile location_meta
    save `location_meta'

// set the connection settings
    quiet run "$prefix/temp/central_comp/libraries/current/stata/create_connection_string.ado"
    create_connection_string, server("modeling-gbd-db") database("gbd") user("dbview") password("E3QNSLvQTRJm")
    local conn_string = r(conn_string)
       
// Set locals
    local metric = "death"
    local measure_id = 1   
    local location_ids = "6 491 492 493 494 495 496 497 498 499 500 501 502 503 504 505 506 507 508 509 510 511 512 513 514 515 516 517 518 519 520 521" 
    // local location_ids = "6 491"
    local year = 1995
    local gbd_id = 4
    local compare_version = 209
    local outdir "$prefix/temp/wgodwin/chn/bar_chart/new"
    local indir "$prefix/temp/wgodwin/chn/bar_chart/new"
    cd "`outdir'"
    local sex_name = "both sexes combined"
    local x_scale_max = 30

// query causes
odbc load, exec("call shared.view_cause_hierarchy_history (shared.active_cause_set_version(3,3))") `conn_string' clear
keep if level == 3 | cause_id == 294
keep cause_id acause acause_parent
compress
tempfile causes
save `causes', replace  
    
// query risks
get_rei_metadata, rei_set_id(1) clear
keep if rei_id == 86 | rei_id == 87 | rei_id == 83 | rei_id == 84 | rei_id == 240 | rei_id == 241 | rei_id == 94 | rei_id == 136 | rei_id == 137 | rei_id == 238 | rei_id == 334 | rei_id == 335 | rei_id == 100
levelsof rei_id, local(risks)
keep rei_id lancet_label
tempfile reilancet
save `reilancet', replace   

get_cause_metadata, cause_set_id(3) clear
keep cause_id lancet_label
tempfile causelancet
save `causelancet', replace

foreach location in `location_ids' {

    // if `loc' == 6 {
    //     local location = "China"
    //     local panel = "(A)"
    //     local x_scale_max = 35
    //     local f_name = "A"
    // }

    // else if `loc' == 492 {
    //     local location = "Beijing"
    //     local panel = "(E)"
    //     local x_scale_max = 35
    //     local f_name = "E"
    // }
    // else if `loc' == 491 {
    //     local location = "Anhui"
    //     local panel = "(F)"
    //     local x_scale_max = 35
    //     local f_name = "F"
    // } 
    // else if `loc' == 518 {
    //     local location = "Tibet"
    //     local panel = "(C)"
    //     local x_scale_max = 40
    //     local f_name = "C"
    // }
    // else if `loc' == 514 {
    //     local location = "Shanghai"
    //     local panel = "(B)"
    //     local x_scale_max = 35
    //     local f_name = "B"
    // }
    // else {
    //     local location = "Sichuan"
    //     local panel = "(D)"
    //     local x_scale_max = 35
    //     local f_name = "D"
    // }
    di in red "Starting on `location'..."
    local fig = "Deaths attributable to risk factors for `location', for both sexes combined, 2016."

    // load results
    get_outputs, topic(rei) measure_id(`measure_id') metric_id(1) year_id(`year') cause_id(all) rei_id(`risks') sex_id(`sex_id') age_group_id(1) location_id(`location') gbd_round_id(`gbd_id') clear

        tempfile risk_data
        save `risk_data', replace
        
        // get all cause, but no risk total
        get_outputs, topic(cause) measure_id(`measure_id') metric_id(1) year_id(`year') cause_id(294) sex_id(`sex_id') age_group_id(1) location_id(`location') gbd_round_id(`gbd_id') clear
        gen rei_id = .
        gen rei_name = "_none"

        append using `risk_data'
        merge m:1 location_id using `location_meta', keep(3) nogen
        local location_na = location_name

        ** merge on causes and drop unnecessary variables
        merge m:1 cause_id using `causes', keep(3) nogen
        keep cause_name cause_id rei_name rei_id sex val acause_parent
        drop if val == .
        drop if acause_parent == "_all" & rei_name != "_none"
        
        // split out the all-cause burden from attributable burden 
        gen total = val if cause_id == 294 & rei_name == "_none"    
            replace total = total[_n-1] if total == .

        //  Divide disaggregated risk dalys by total all cause dalys to get percentage of dalys
        gen perc_measure = 100*(val/total)
        gsort -perc_measure
        // drop if perc_measure < 1
        
        // calculate total attributable burden per risk/sex/year:
        sort rei_name sex 
        by rei_name: egen total_atrib = total(perc_measure)
     
        ** reshape by cause for graph
        // excluding following causes because they're not outcomes: "Neglected tropical diseases and malaria","Neonatal disorders", "Forces of nature, war, and legal intervention"
        drop if inlist(cause_id, 294, 728, 344)

        //add lancet names
        merge m:1 rei_id using `reilancet', keep(3) nogen
        drop rei_name
        rename lancet_label rei_name
        merge m:1 cause_id using `causelancet', keep(3) nogen
        drop cause_name
        rename lancet_label cause_name

        // generate a cause order variable for the legend
        gen cause_order = .  
        replace cause_order = 1 if cause_name == "Neonatal preterm birth"
        replace cause_order = 2 if cause_name == "Neonatal encephalopathy due to birth asphyxia and trauma"
        replace cause_order = 3 if cause_name == "Other neonatal disorders"        
        replace cause_order = 4 if cause_name == "Lower respiratory infections"
        replace cause_order = 5 if cause_name == "Neonatal sepsis and other neonatal infections"
        replace cause_order = 6 if cause_name == "Diarrhoeal diseases"
        replace cause_order = 7 if cause_name == "Hemolytic disease and other neonatal jaundice"
        //replace cause_order = 8 if cause_name == "Iron-deficiency anaemia"
        replace cause_order = 8 if cause_name == "Meningitis"
        replace cause_order = 9 if cause_name == "Upper respiratory infections"
        replace cause_order = 10 if cause_name == "Sudden infant death syndrome"
        replace cause_order = 11 if cause_name == "Protein-energy malnutrition"
        // replace cause_order = 13 if cause_name == "Drug use disorders"
        replace cause_order = 12 if cause_name == "Encephalitis"
        // replace cause_order = 15 if cause_name == "HIV/AIDS"
        replace cause_order = 13 if cause_name == "Measles"
        replace cause_order = 14 if cause_name == "Otitis media"

        drop if cause_order == . 
        // TEMPORARY //
        drop cause_name acause cause_id val
        rename perc_measure val_
        reshape wide val_, i(rei_id rei_name sex total total_atrib) j(cause_order)

        ** replace missing with 0
        foreach var of varlist val* {
            replace `var' = 0 if `var' == .
        }
        
        // order risks by total attributable burden
        gsort -total_atrib
        gen order = _n
        
        // create holding variables if the variable is currently empty
        //foreach num of numlist 1/19 {
        //    capture gen val_`num' = 0
        // }
        order sex rei* total* order val_1 val_2 val_3 val_4 val_5 val_6 val_7 val_8 val_9 val_10 val_11 val_12 val_13 val_14 // val_13 val_14 val_15 val_16 val_17

        export delimited using "`indir'/inputs/figure_2_`location'_`year'.csv", replace
        
        if `location' == 6 {
            pdfstart using "`outdir'/Figure_2_`location'_`year'.pdf"
        }

        graph hbar val*,  over(rei_name, sort(order) label(labsize(vsmall)) gap(5)) stack ///
        bar(1, color("191 0 44")) bar(2, color("244 109 67")) bar(3, color("253 174 97")) ///
        bar(4, color("255 229 204")) bar(5, color("166 217 106")) bar(6, color("26 152 80")) bar(7, color("0 68 27")) ///
        bar(8, color("199 234 229")) bar(9, color("128 205 193")) bar(10, color("53 151 143")) ///
        bar(11, color("218 165 32")) /// bar(12, color("251 200 227")) bar(13, color("202 194 126")) bar(14, color("255 210 0")) ///
        title("`location_na'") ///
        ytitle("Percent of Total Under 5 Deaths", size(vsmall)) ylabel(0(2.5)`x_scale_max', labsize(vsmall)) ///
        legend(symy(*.5) symx(*.2) size(tiny) ring(0) position(4) margin(tiny) colfirst region(lwidth(none)) label(1 "Neonatal preterm birth complications") label(2 "Neonatal encephalopathy due to birth asphyxia and trauma") label(3 "Other neonatal disorders") label(4 "Lower respiratory infections") label(5 "Neonatal sepsis and other neonatal infections") label(6 "Diarrhoeal diseases") label(7 "Hemolytic disease and other neonatal jaundice") label(8 "Meningitis") label(9 "Upper respiratory infections") label(10 "Sudden infant death syndrome") label(11 "Protein-energy malnutrition") label(12 "Encephalitis") label(13 "Measles") label(14 "Otitis media")) ///
        graphregion(color(white)) plotregion(margin(0) lwidth(vthin))

        pdfappend

    }
    pdffinish, view

//  bar(13, color("222 119 174")) bar(14, color("197 27 125")) bar(15, color("64 0 75")) bar(16, color("153 112 171")) bar(17, color("194 165 207")) bar(18, color("69 117 180")) bar(19, color("41 70 108")) 
