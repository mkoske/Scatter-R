library('RUnit')

# ##
# Test suite for Scatter algorithm and related helper functions
# 
# See:
# https://cran.r-project.org/web/packages/RUnit/RUnit.pdf and
# http://www.johnmyleswhite.com/notebook/2010/08/17/unit-testing-in-r-the-bare-minimum/
# ##
tests <- defineTestSuite(
	'scatter',
	dirs = 'algorithm',
	testFileRegexp = '^test\\-(.*)\\.R$'
    )

results <- runTestSuite(tests)
printTextProtocol(results)


