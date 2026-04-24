context('apply_rt_window_to_annotation()')

skip_if_not_installed('faahKO', minimum_version = '1.18.0')
library(faahKO)

## Minimal annotation with uROI + FIR populated so we can see slot-level writes.
spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                  system.file('cdf/KO/ko16.CDF', package = "faahKO"))

targetFeatTable <- data.frame(matrix(vector(), 2, 8,
    dimnames = list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax",
                           "mzMin", "mz", "mzMax"))),
    stringsAsFactors = FALSE)
targetFeatTable[1, ] <- c("ID-1", "Cpd 1", 3310, 3344.888, 3390,
                           522.194778, 522.2, 522.205222)
targetFeatTable[2, ] <- c("ID-2", "Cpd 2", 3280, 3385.577, 3440,
                           496.195038, 496.2, 496.204962)
targetFeatTable[, 3:8] <- vapply(targetFeatTable[, 3:8], as.numeric,
                                  FUN.VALUE = numeric(2))

uROIseed <- data.frame(rtMin = c(3310, 3280), rt = c(3345, 3385),
                       rtMax = c(3390, 3440),
                       mzMin = c(522.19, 496.19), mz = c(522.20, 496.20),
                       mzMax = c(522.21, 496.21))

FIRseed <- data.frame(rtMin = c(3310, 3280), rtMax = c(3390, 3440),
                      mzMin = c(522.19, 496.19), mzMax = c(522.21, 496.21))

baseAnnot <- peakPantheRAnnotation(spectraPaths = spectraPaths,
    targetFeatTable = targetFeatTable,
    uROI = uROIseed, FIR = FIRseed, uROIExist = TRUE)

newRt <- c(3320, 3380)


test_that('bound-order guard rejects rt[1] >= rt[2]', {
    expect_error(apply_rt_window_to_annotation(baseAnnot, 1, c(3400, 3300)),
                 "strictly less")
    expect_error(apply_rt_window_to_annotation(baseAnnot, 1, c(3300, 3300)),
                 "strictly less")
})

test_that('non-finite or wrong-length rt rejected', {
    expect_error(apply_rt_window_to_annotation(baseAnnot, 1, c(NA, 3400)),
                 "finite")
    expect_error(apply_rt_window_to_annotation(baseAnnot, 1, 3300),
                 "length-2")
})

test_that('cpdNb guard rejects out-of-range index', {
    expect_error(apply_rt_window_to_annotation(baseAnnot, 0, newRt))
    expect_error(apply_rt_window_to_annotation(baseAnnot, 99, newRt))
})

test_that('invalid targets rejected', {
    expect_error(apply_rt_window_to_annotation(baseAnnot, 1, newRt,
                                               targets = character(0)))
    expect_error(apply_rt_window_to_annotation(baseAnnot, 1, newRt,
                                               targets = c("ROI")))
    expect_error(apply_rt_window_to_annotation(baseAnnot, 1, newRt,
                                               targets = c("uROI", "bogus")))
})

test_that('default targets update uROI + FIR, leave ROI untouched', {
    out <- apply_rt_window_to_annotation(baseAnnot, 1, newRt)
    expect_equal(uROI(out)[1, "rtMin"], newRt[1])
    expect_equal(uROI(out)[1, "rtMax"], newRt[2])
    expect_equal(FIR(out)[1, "rtMin"], newRt[1])
    expect_equal(FIR(out)[1, "rtMax"], newRt[2])
    # ROI is the acquisition-time slot, never touched
    expect_equal(ROI(out)[1, "rtMin"], ROI(baseAnnot)[1, "rtMin"])
    expect_equal(ROI(out)[1, "rtMax"], ROI(baseAnnot)[1, "rtMax"])
    # Other compound row untouched
    expect_equal(uROI(out)[2, "rtMin"], uROI(baseAnnot)[2, "rtMin"])
    expect_equal(FIR(out)[2, "rtMin"], FIR(baseAnnot)[2, "rtMin"])
})

test_that('targets = "uROI" only updates uROI', {
    out <- apply_rt_window_to_annotation(baseAnnot, 1, newRt,
                                         targets = "uROI")
    expect_equal(uROI(out)[1, "rtMin"], newRt[1])
    expect_equal(FIR(out)[1, "rtMin"], FIR(baseAnnot)[1, "rtMin"])
    expect_equal(FIR(out)[1, "rtMax"], FIR(baseAnnot)[1, "rtMax"])
})

test_that('targets = "FIR" only updates FIR', {
    out <- apply_rt_window_to_annotation(baseAnnot, 1, newRt,
                                         targets = "FIR")
    expect_equal(FIR(out)[1, "rtMin"], newRt[1])
    expect_equal(uROI(out)[1, "rtMin"], uROI(baseAnnot)[1, "rtMin"])
    expect_equal(uROI(out)[1, "rtMax"], uROI(baseAnnot)[1, "rtMax"])
})

## Flag updates: editing a slot must flip the corresponding use* flag so the
## next refit actually reads the new bounds (previously the edit was silently
## ignored when the object was previously run with useUROI/useFIR = FALSE).

# Build a variant with the flags off to verify they get flipped
offFlagsAnnot <- suppressMessages(peakPantheRAnnotation(
    spectraPaths = spectraPaths,
    targetFeatTable = targetFeatTable,
    uROI = uROIseed, FIR = FIRseed,
    uROIExist = FALSE, useUROI = FALSE, useFIR = FALSE))

test_that('editing uROI flips @useUROI and @uROIExist to TRUE', {
    expect_false(useUROI(offFlagsAnnot))
    expect_false(uROIExist(offFlagsAnnot))
    out <- apply_rt_window_to_annotation(offFlagsAnnot, 1, newRt,
                                         targets = "uROI")
    expect_true(useUROI(out))
    expect_true(uROIExist(out))
    # FIR flag untouched when only uROI edited
    expect_false(useFIR(out))
})

test_that('editing FIR flips @useFIR to TRUE', {
    expect_false(useFIR(offFlagsAnnot))
    out <- apply_rt_window_to_annotation(offFlagsAnnot, 1, newRt,
                                         targets = "FIR")
    expect_true(useFIR(out))
    # uROI flags untouched when only FIR edited
    expect_false(useUROI(out))
    expect_false(uROIExist(out))
})

test_that('default targets flip both @useUROI and @useFIR', {
    out <- apply_rt_window_to_annotation(offFlagsAnnot, 1, newRt)
    expect_true(useUROI(out))
    expect_true(uROIExist(out))
    expect_true(useFIR(out))
})

test_that('rt values outside ROI are clipped to ROI bounds', {
    # Compound 1 ROI: rtMin=3310, rtMax=3390
    out <- apply_rt_window_to_annotation(baseAnnot, 1, c(3200, 3500))
    expect_equal(uROI(out)[1, "rtMin"], 3310)
    expect_equal(uROI(out)[1, "rtMax"], 3390)
    expect_equal(FIR(out)[1, "rtMin"], 3310)
    expect_equal(FIR(out)[1, "rtMax"], 3390)
})

test_that('selection entirely outside ROI returns annotation unchanged', {
    # Both values below ROI rtMin=3310
    out <- apply_rt_window_to_annotation(baseAnnot, 1, c(3100, 3200))
    expect_equal(uROI(out)[1, "rtMin"], uROI(baseAnnot)[1, "rtMin"])
    expect_equal(uROI(out)[1, "rtMax"], uROI(baseAnnot)[1, "rtMax"])
    # Both values above ROI rtMax=3390
    out <- apply_rt_window_to_annotation(baseAnnot, 1, c(3400, 3500))
    expect_equal(FIR(out)[1, "rtMin"], FIR(baseAnnot)[1, "rtMin"])
    expect_equal(FIR(out)[1, "rtMax"], FIR(baseAnnot)[1, "rtMax"])
})
