* Stata Outputs
* Autor: Diana Pérez Lopez
* 		 diana.perezlopez@yale.edu
* 		 dk.perez10@uniandes.edu.co
* Date: April 3rd, 2019
* Last update: October 9th, 2022
clear
cap log close
set more off
cls


if "`c(username)'"=="dk.perez10" | "`c(username)'"=="Diana" | "`c(username)'"=="dkp28" {
	cd "C:/Users//`c(username)'/Dropbox/Website/Output"
}
else {
	*cd "INCLUDE YOUR DIRECTORY"
}
sysuse nlsw88.dta, clear
describe

*********************************************************************************
** INDEX 																	   **
*********************************************************************************
{
* I. 	TABULATE
* II. 	TABLE WITH DESCRIPTIVE STATISTICS (MEAN, SD, MIN, MAX)
* III. 	MEAN DIFFERENCE TABLE
* IV. 	SCATTER PLOTS
* V. 	BAR GRAPHS
* VI. 	DENSITY GRAPGHS (FOR CONTINUOUS VARIABLES)
}

*********************************************************************************
** I. TABULATE 		    											       	   **
*********************************************************************************
{
*	1. One-way tables of frequencies
* -------------------------------------------------------------------------------
	/*  |Variable |frequency | % |
		|Cat. 1   | ...
		|Cat. 2	  | ...															*/

*) Option 1: using matrices (commands: frmttable and putexcel)

	* Matrix with frequencies
	cap mat drop cell // deleting the matrix
	tabulate race, matcell(cell) // cell: matrix with frequencies 
	mat l cell // inspect the matrix 
	
	* Define matrix for the table
	levelsof race // to extract the # of categories
	return list // check description of r(r)
	mat define MAT_tabla = J(r(r)+1 ,2 ,.) // matrix with 
		// one row per categorie + other wor for totals
		// two columns: one for fequency, one for %
		// the dot indicates that all entries should be missing
	
	* Saving the N obs. and cummulated %
	qui count if race < . // N obs. with information for race
	return list // check description of r(N)
	mat MAT_tabla[rowsof(MAT_tabla), 1] = r(N) // [last row, 1st col]
	mat MAT_tabla[rowsof(MAT_tabla), 2] = 100 // [last row, 2nd col]
	
	* Frequencies
	mat MAT_tabla[1,1] = cell[1..rowsof(cell),1] // [from row 1 to last wor, 1st col]
	
	* Percentages
	local l = rowsof(MAT_tabla) - 1 // N. of rows - 1
	foreach j of numlist 1/`l' { // repet the procedure for each row but the last one
		mat MAT_tabla[`j',2] = (MAT_tabla[`j',1]/MAT_tabla[rowsof(MAT_tabla),1])*100
	}
	
	* Column and row titles
	mat coln MAT_tabla = "Freq." "%" // col titles
	mat rown MAT_tabla = "White" "Black" "Other" "Total" // row titles
	mat l MAT_tabla
	
	* Output 1: Putexcel (only for excel files)
	h putexcel
	putexcel set "TBL_Tab_Race_putexcel.xlsx", replace sheet(Uno)
	putexcel A2 = "Race distribution", left vcenter
	putexcel B4 = matrix(MAT_tabla), colnames left vcenter
	putexcel A5 = matrix(MAT_tabla), rownames left vcenter
	putexcel B5 = matrix(MAT_tabla), nformat(number_sep_d2) left vcenter

	* Output 2. Frmttable (recomendado para doc y tex)
	/* You should previously install the command: 
	   1. Execute the command "findit frmttable" This will bring up a list of packages to install. I think you want the one starting with sg97_5.
	   2. Choose "click here to install", and when it's done, click "click here to return to the previous screen"
	   3. Look for "ancilliary files" at the bottom. Click on "click here to get"
	*/
	h frmttable
	frmttable using "TBL_Tab_Race_frmttable.doc", replace sdec(2) 				///
			  statmat(MAT_tabla)
	
*) Option 2: direct (command: tabout, usefull for tex, html, or ugly csv)
ssc install tabout
h tabout
tabout south using "TBL_Tab_Race_tabout.tex", replace cells(freq cell) 			///
	   format(0c 2p 2p) clab("Freq." "%") style(tex)

	   
*	2. Two-way tables of frequencies
* -------------------------------------------------------------------------------
	/*  |Variable 	  | V2. Cat. 1 | V2. Cat. 2 |
		|V1. Cat. 1   | ...
		|V1. Cat. 2	  | ...														*/

*) Option 1: using matrices (commands: frmttable and putexcel)

	* Matrix with frequencies
	cap mat drop MAT_tabla
	tab race south, matcell(MAT_tabla)
	mat rownames MAT_tabla = "White" "Black" "Other"
	mat colnames MAT_tabla = "South" "Not south"
	
	* Output 1. Putexcel
	putexcel set "TBL_Tab_RaceSouth_putexcel.xlsx", replace sheet(First)
	putexcel A2=" Race distribution by region", left vcenter
	putexcel B4 = matrix(MAT_tabla), colnames left vcenter
	putexcel A5 = matrix(MAT_tabla), rownames left vcenter
	putexcel B5 = matrix(MAT_tabla), nformat(number_sep_d2) left vcenter
	
	* Output 2. Frmttable
	frmttable using "TBL_Tab_RaceSouth_frmttable.doc", replace sdec(2) 			///
		statmat(MAT_tabla) title("Tabla X.")
	
*) Option 2: direct (command: tabout, usefull for tex, html, or ugly csv)
tabout south married using "TBL_Tab_RaceSouth_tabout.tex", replace 				///
	cells(freq col row) format(0c 2p 2p) clab("Freq." "%C" "%F") 				///
	layout(cb) style(tex)
}


*********************************************************************************
** II. 	TABLE WITH DESCRIPTIVE STATISTICS (MEAN, SD, MIN, MAX)				   **
*********************************************************************************
{
	/* VARIABLE | N | Mean | SD | Min | Max 
	   Var 1	| ...
	   Var 2	| ...															*/
	
*	1. Using matrices (commands: frmttable and putexcel)
* -------------------------------------------------------------------------------
	
	* Matrix definition
	local vars = "grade wage hours tenure"
	local nv: word count `vars'
	
	mat MAT_tabla = J(`nv',5,.)
	
	* Including the statistics
	local n = 0
	foreach var of varlist `vars' {
		local ++n
		qui sum `var'
		return list // check r(N), r(mean), r(sd), ...
		mat MAT_tabla[`n',1] = (r(N),r(mean),r(sd),r(min),r(max))
	}
	
	* Column and row titles
	mat coln MAT_tabla = "N" "Mean" "SD" "Min" "Max" 
	mat rown MAT_tabla = "Grade" "Wage" "Hours" "Tenure"

	* Output 1. Putexcel
	putexcel set "TBL_DS_putexcel.xlsx", replace sheet(Uno)
	putexcel A2 = "Any title", left vcenter
	putexcel B4 = matrix(MAT_tabla), colnames left vcenter
	putexcel A5 = matrix(MAT_tabla), rownames left vcenter
	putexcel B5 = matrix(MAT_tabla), nformat(number_sep_d2) left vcenter
			
	* Output 2. Frmttable
	frmttable using "TBL_DS_frmttable.doc", replace sdec(2) statmat(MAT_tabla)
	
*	2. Direct: (command: outreg2, good for excel, doc, and tex)
* -------------------------------------------------------------------------------
ssc install outreg2
h outreg2 
outreg2 using "TBL_DS_outreg2.doc", replace sum(log) 							///
		eqkeep(N mean sd min max) keep(grade wage hours tenure) decmark(.) 		///
		label // open the file from your folder!
	
}


*********************************************************************************
** III. 	MEAN DIFFERENCE TABLE											   **
*********************************************************************************
{
	/* 	     | N | Mean (C) | SD (C) | Mean (T) | SD (T) | Mean diff. (T-C) | P-val
	   Var 1 | ...
	   Var 2 | ...																*/
	
*	1. Using matrices (commands: frmttable and putexcel)
* -------------------------------------------------------------------------------

	* Matrix definition
	local vars = "grade wage hours tenure"
	local nv: word count `vars'
	
	mat MAT_tabla = J(`nv',7,.)

	* + Information
	local n=0
	foreach var of varlist `vars' {
		local ++n
		ttest `var', by(south) unequal
		return list
		mat MAT_tabla[`n',1] = (r(N_1)+r(N_2), r(mu_1), r(sd_1), 				///
								r(mu_2), r(sd_2), r(mu_2)-r(mu_1), r(p))
	}
		// scalars that end with _2 is the information for the group whose category has the higest value
	
	* Column and row titles
	mat coln MAT_tabla = "N" "Mean (NS)" "SD (NS)" "Mean (S)" "SD (S)" 			///
						 "Diff. (S-NS)" "P-value" 
	mat rown MAT_tabla = "Grade" "Wage" "Hours" "Tenure"

	* Output 1. Putexcel
	putexcel set "TBL_MD_bySouth_putexcel.xlsx", replace sheet(Uno)
	putexcel A2 = "Title", left vcenter
	putexcel B4 = matrix(MAT_tabla), colnames left vcenter
	putexcel A5 = matrix(MAT_tabla), rownames left vcenter
	putexcel B5 = matrix(MAT_tabla), nformat(number_sep_d2) left vcenter
			
	* Output 2. Frmttable
	frmttable using "TBL_MD_bySouth_frmttable.doc", replace sdec(2) 			///
		statmat(MAT_tabla)

*	2. Direct (almost): (command: esttab, only for tex)
* -------------------------------------------------------------------------------
quiet estpost tabstat grade wage hours tenure, by(south) statistics(mean sd) 	///
	columns(statistics) nototal
esttab using "TBL_MD_bySouth_esttab.tex", replace main(mean) aux(sd) brackets 	///
	nostar unstack  label

quiet estpost ttest grade wage hours tenure, by(south)
esttab using "TBL_MD_bySouth_esttab.tex", append star(* 0.10 ** 0.05 *** 0.01)
}


*********************************************************************************
** IV. 	SCATTER PLOTS														   **
*********************************************************************************
{
// Run the code and then look on the helpfile for the desciprtion of each option (or google it!)
// then run the code without the option and compare :)
tw (scatter wage tenure if south==1, color(ltblue)) 							///
   (scatter wage tenure if south==0, color(gray) mcolor(%30)) 					///
   (lfit wage tenure if south==1, color(navy) lw(0.4)) 							///
   (lfit wage tenure if south==0, color(black) lp(".-.-") lw(0.4)),				///
    legend(lab(1 "Wage: South") lab(2 "Wage: Not south") 						///
	lab(3 "Fitted values: Sur") lab(4 "Fitted values: Not south")) 				///
	ytitle("USD") xtitle("Years") graphregion(fcolor(white)) 					///
	title("Wage vs Tenure")

// scatter: print the dots
// lfit: includes the fitted values from a linear model
gr export "GR_Scatter_WageTenurebySouth.tif", replace as(tif)
gr export "GR_Scatter_WageTenurebySouth.pdf", replace as(pdf)
}


*********************************************************************************
** V. 	BAR GRAPHS															   **
*********************************************************************************
{
* 	1. Distribution of a categorical variable
* -------------------------------------------------------------------------------

	* One temporal variable for non graduated
	tempvar temp1
	gen `temp1'=1 - collgrad
	
	* Graph
	gr bar collgrad `temp1', blabel(bar, format(%8.1f)) 						///
		graphregion(fcolor(white)) legend(r(1) 									///
		label(1 "College graduates") label(2 "HS graduates")) 					///
		bar(1, color(ltblue)) bar(2,color(gray)) percentages 					///
		ti("Title :)")
	gr export "GR_Bar_Collegecompletion.tif", replace as(tif)
	gr export "GR_Bar_Collegecompletion.pdf", replace as(pdf)
	cap drop `temp1'
		
* 	2. One categorical vs another categorial
* -------------------------------------------------------------------------------
		
	* Variable otra categoria
	// as temp1 is temporal you need to run lines 296 - 308 at the same time
	tempvar temp1
	gen `temp1'=abs(1-collgrad)

	* Gráfico
	gr bar collgrad `temp1', blabel(bar, format(%8.1f)) 						///
		graphregion(fcolor(white)) 												///
		legend(r(1) label(1 "College graduates") label(2 "HS graduates")) 		///
		over(south, relabel(1 "Non south" 2 "South")) ytitle("Fraction") 		///
		bar(1, color(emidblue)) bar(2, color(gray))								///
		title("Graduation rate by region") percentages
	gr export "GR_Bar_CollegecompletionbySouth.tif", replace as(tif)
	gr export "GR_Bar_CollegecompletionbySouth.pdf", replace as(pdf)
	cap drop `temp1'

	* Graph with confidence intervals: not very important
	preserve
		replace collgrad=collgrad*100

		statsby mean = r(mean) ub = r(ub) lb = r(lb) N = r(N), by(south) clear: ///
			ci mean collgrad	

		gen eti=string(mean, "%4.2f")
		twoway bar mean south, color(gray) barw(0.5) || 						///
			rcap ub lb south, lcolor(black) lwidth(0.5) || 						///
			scatter mean south, ms(none) mlab(eti) mlabpos(11) mlabcolor(black) ///
			legend(off) xlabel(0 "Otro" 1 "Sur") xti("") 						///
			graphregion(fcolor(white)) title("Graduación vs Región") 			///
			ytitle("Porcentaje")

		gr export "GR_Bar_CollegecompletionbySouth_PlusCI.tif", as(tif) replace
		gr export "GR_Bar_CollegecompletionbySouth_PlusCI.pdf", as(pdf) replace
	restore

* 	3. Mean of contuous variable by categorical variable
* -------------------------------------------------------------------------------
gr bar (mean) wage, blabel(bar, format(%8.1f)) graphregion(fcolor(white)) 		///
	over(south, relabel(1 "Not south" 2 "South")) bar(1, color(edkblue)) 		///
	ytitle("USD") title("Wage by region")

gr export "GR_Bar_WagebySouth.tif", replace as(tif)
gr export "GR_Bar_WagebySouth.pdf", replace as(pdf)
}


*********************************************************************************
** VI. 	DENSITY GRAPGHS (FOR CONTINUOUS VARIABLES)							   **
*********************************************************************************
{
*) Histogram
hist wage, color(edkblue) graphregion(fcolor(white)) xtitle("Wage") 			///
	ytitle("Density") title("Title :)")
gr export "GR_Density_Hist_Wage.tif", replace as(tif)
gr export "GR_Density_Hist_Wage.pdf", replace as(pdf)
	
*) Kernel density
kdensity wage, lc(black) xline(10, lcolor(black)) 								///
	graphregion(fcolor(white)) note("") xtitle("Wage") 							///
	ytitle("Density") title("Kernel density")
gr export "GR_Density_Kernel_Wage.tif", replace as(tif)
gr export "GR_Density_Kernel_Wage.pdf", replace as(pdf)
}

