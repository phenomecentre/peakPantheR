context('annotation_showText_UI_helper()')

# UI specific function that returns a textual description of an annotation to show in UI

test_that('UI show text outputs right values', {
  # Input and expected
  properties_default <- list(nbCompounds = 0,
                             nbSamples = 0,
                             uROIExist = FALSE,
                             useUROI = FALSE,
                             useFIR = FALSE,
                             isAnnotated = FALSE)
  properties_ROIFIRInit <- list(nbCompounds = 2,
                                nbSamples = 3,
                                uROIExist = TRUE,
                                useUROI = TRUE,
                                useFIR = TRUE,
                                isAnnotated = TRUE)
  expected_text_default    <- list('Not annotated', '0 compounds', '0 samples', 'updated ROI do not exist (uROI)', 'does not use updated ROI (uROI)', 'does not use fallback integration regions (FIR)')
  expected_text_ROIFIRInit <- list('Is annotated',  '2 compounds', '3 samples', 'updated ROI exist (uROI)',        'uses updated ROI (uROI)',         'uses fallback integration regions (FIR)')

  # default values, no uROI, no use of FIR
  result_default  <- evaluate_promise(annotation_showText_UI_helper(properties_default))
  expect_equal(result_default$result, expected_text_default)

  # multiple compounds and spectra with uROIExist and useFIR
  result_ROIFIRInit <- evaluate_promise(annotation_showText_UI_helper(properties_ROIFIRInit))
  expect_equal(result_ROIFIRInit$result, expected_text_ROIFIRInit)
})