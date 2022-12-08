* Matrix with categorical variables distribution
* Author: Perez Lopez, Diana
* 		  diana.perezlopez@yale.edu

cap program drop cat_distrib
program define cat_distrib

	version 15
	syntax varlist(numeric), [val1(integer 0) val2(integer 5) val3(integer 10)]

	/* 
	INPUT
	
	varlist: List of variable names of items to be presented.
	
	OUTPUT
	
	MAT_table: matrix with N and distribution.
	*/

	* Levels of the variable 
	* ---------------------------------------------------------------------------
	local nmaxcat = 0
	foreach var of varlist `varlist' {
		
		qui levelsof `var', local(levels)
		qui local nacat: word count `levels'
		if `nmaxcat' < `nacat' qui levelsof `var', local(maxlevels)
		local nmaxcat = max(`nmaxcat', `nacat')
	}
	
	* Matrix definition
	* ---------------------------------------------------------------------------
	local rows : word count `varlist'
	local cols = `nmaxcat' + 1
	mat MAT_table = J(`rows', `cols', .)

	* Filling the matrix
	* ---------------------------------------------------------------------------
	local r = 0
	foreach var of varlist `varlist' {
		
		local ++r
		
		*) N
		qui count if `var' != .
		mat MAT_table[`r', 1] = `r(N)'
		
		*) Filling %
		local c = 1
		foreach j of numlist `maxlevels' {
			
			local ++c

			qui count if `var' == `j'
			mat MAT_table[`r', `c'] = (`r(N)'/MAT_table[`r', 1])*100
		}

	}

	* Matrix labels
	* ---------------------------------------------------------------------------
	
	*) Row names
	mat rownames MAT_table = `varlist'
	
	*) Column names
	mat colnames MAT_table = "N" `maxlevels'

end
