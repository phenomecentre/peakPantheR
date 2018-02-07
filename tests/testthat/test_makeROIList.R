context('makeROIList()')

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)

test_that('a ROIList is generated', {

	# use ko15.CDf file from the pkg faahKO
	singleSpectraDataPath <- system.file('cdf/KO/ko15.CDF', package = "faahKO")
	raw_data  						<- MSnbase::readMSData(singleSpectraDataPath, centroided=TRUE, mode='onDisk')

	# targeted features in faahKO
	targetFeatTable     	<- data.frame(matrix(vector(), 4, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=F)
	targetFeatTable[1,] 	<- c(1, "testCpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
	targetFeatTable[2,] 	<- c(2, "testCpd 2, 2 peaks in box", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
	targetFeatTable[3,] 	<- c(3, "testCpd 3", 3420., 3454.435, 3495., 464.195358, 464.2, 464.204642)
	targetFeatTable[4,] 	<- c(4, "testCpd 4", 3670., 3701.697, 3745., 536.194638, 536.2, 536.205362)
	targetFeatTable[,c(1,3:8)] <- sapply(targetFeatTable[,c(1,3:8)], as.numeric)

	# expected ROIList
	expected_ROIList <- list(list("mz"=522.2, "mzmin"=522.194778, "mzmax"=522.205222, "scmin"=518, "scmax"=569, "length"=-1, "intensity"=-1),
	                         list("mz"=496.2, "mzmin"=496.195038, "mzmax"=496.204962, "scmin"=499, "scmax"=601, "length"=-1, "intensity"=-1),
	                         list("mz"=464.2, "mzmin"=464.195358, "mzmax"=464.204642, "scmin"=588, "scmax"=636, "length"=-1, "intensity"=-1),
	                         list("mz"=536.2, "mzmin"=536.194638, "mzmax"=536.205362, "scmin"=748, "scmax"=796, "length"=-1, "intensity"=-1))

	# result ROIList
	result_ROIList <- makeROIList(raw_data, targetFeatTable)

  expect_equal(result_ROIList, expected_ROIList)
})
