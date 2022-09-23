test_that("find_xpath works on raw text", {
  test_xml <- '
  <p>
  <w pos="at">The</w>
  <w pos="nn">example</w>
  <punct>.</punct>
  </p>'
  
  expect_length(find_xpath(test_xml, "//w"), 2)
  expect_s3_class(find_xpath(test_xml, "//w"), "xml_nodeset")
  expect_match(
    as.character(find_xpath(test_xml, "//@pos")[[1]]),
    "pos=\"at\"")
  noun <- find_xpath(test_xml, "//w[@pos='nn']")
  expect_length(noun, 1)
  expect_match(as.character(noun),
               "<w pos=\"nn\">example</w>")
  
  texts <- find_xpath(test_xml, "//w", fun = xml2::xml_text)
  expect_length(texts, 2)
  expect_type(texts, "character")
  pos <- find_xpath(test_xml, "//w", fun = xml2::xml_attr, attr = "pos")
  expect_match(pos[[1]], "at")
  expect_length(pos, 2)
})

test_that("find_xpath works on files", {
  expect_length(find_xpath(xml_corp[[1]], "//d1:w"), 70)
  from_corpus <- find_xpath(xml_corp, "//d1:w")
  expect_length(from_corpus, 147)
  expect_s3_class(from_corpus, "xml_nodeset")
  expect_length(find_xpath(xml_corp, "//w"), 0)
})

test_that("find_path throws the right errors", {
  expect_error(find_xpath(xml_corp), "missing, with no default")
  expect_error(find_xpath("not xml", "//w"), "does not exist")
  expect_error(find_xpath(3, "//w"), "must be a character vector")
})

test_that("mclm_xml_text works", {
  test_xml <- '
  <p>
  <w pos="at">The</w>
  <w pos="nn">example</w>
  <punct>.</punct>
  </p>'
  expect_match(
    mclm_xml_text(find_xpath(test_xml, "/")),
    "The example ."
  )
  expect_match(
    mclm_xml_text(find_xpath(xml_corp[[1]], "/")),
    "Sample A01 from "
  )
})
