context('spectraPaths_and_metadata_UI_helper()')

# test only spectraPaths
# test only spectraMetadataPaths (or both)
# test error

## Input data
# spectraPath
input_spectraPaths    <- c('./path/file1', './path/file2', './path/file3')

# spectraMetadata
input_spectraMetadata <- data.frame(matrix(data=c(input_spectraPaths, c('a','b','c')), nrow=3, ncol=2, dimnames=list(c(),c('filepath', 'testcol')), byrow=FALSE), stringsAsFactors=FALSE)

# temporary files
spectraMetaPath       <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
utils::write.csv(input_spectraMetadata, file=spectraMetaPath, row.names=FALSE)


test_that('spectraPaths, no spectraMetadata', {
    # expected
    expected    <- list(spectra=input_spectraPaths, meta=NULL)

    # results (output, warnings and messages)
    result_load <- evaluate_promise(spectraPaths_and_metadata_UI_helper(spectraPaths = input_spectraPaths,
                                                                        spectraMetadataPath = NULL))
    # Check result
    expect_equal(result_load$result, expected)

    # Check result messages (in output)
    expect_equal(length(result_load$messages), 0)
    expect_equal(result_load$output, "")
})

test_that('no spectraPaths, spectraMetadata', {
    # expected
    expected    <- list(spectra=input_spectraPaths, meta=input_spectraMetadata[,'testcol', drop=FALSE])

    # results (output, warnings and messages)
    result_load <- evaluate_promise(spectraPaths_and_metadata_UI_helper(spectraPaths = NULL,
                                                                        spectraMetadataPath = spectraMetaPath))
    # Check result
    expect_equal(result_load$result, expected)

    # Check result messages (in output)
    expect_equal(length(result_load$messages), 0)
    expect_equal(result_load$output, "")
})

test_that('spectraPaths and spectraMetadata', {
    # If both are provided, only use the spectraMetadata (different size between both to ensure we have the right one)

    # expected
    expected    <- list(spectra=input_spectraPaths, meta=input_spectraMetadata[,'testcol', drop=FALSE])

    # results (output, warnings and messages)
    result_load <- evaluate_promise(spectraPaths_and_metadata_UI_helper(spectraPaths = input_spectraPaths[1:2],
                                                                        spectraMetadataPath = spectraMetaPath))
    # Check result
    expect_equal(result_load$result, expected)

    # Check result messages (in output)
    expect_equal(length(result_load$messages), 0)
    expect_equal(result_load$output, "")
})

test_that('raise errors', {

    noFile  <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')

    wrong_spectraMetadata <- input_spectraMetadata[,'testcol', drop=FALSE]
    wrong_spectraMetaPath <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
    utils::write.csv(wrong_spectraMetadata, file=wrong_spectraMetaPath, row.names=FALSE)

    # Both inputs are NULL
    msg1    <- paste('Error: spectraPaths and spectraMetadataPath are not set', sep='')
    expect_error(spectraPaths_and_metadata_UI_helper(NULL, NULL), msg1, fixed=TRUE)

    # spectraMetadata CSV file doesn't exist
    msg2    <- paste('Error: spectraMetadata file does not exist', sep='')
    expect_error(spectraPaths_and_metadata_UI_helper(NULL, spectraMetadataPath=noFile), msg2, fixed=TRUE)

    # spectraMetadata does not have a 'filepath' column
    msg3    <- paste("Error: the column 'filepath' must be present in the spectraMetadata", sep='')
    expect_error(spectraPaths_and_metadata_UI_helper(NULL, spectraMetadataPath=wrong_spectraMetaPath), msg3, fixed=TRUE)
})
