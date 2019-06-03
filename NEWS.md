NEWS
================
R. Mark Sharp
04/18/2019

# nprcmanager 0.5.15 (20190602)

  - Added ability to create an example pedigree file using the
    **examplePedigree** data structure.
  - Added **summary.nprcmanagGV** and **print.summary.nprcmangGV**
    functions

# nprcmanager 0.5.14 (20190518)

  - Added ability to use Excel files as input
      - Added getGenotypes, getPedigree, getBreederPed,
        readExcelPOSIXToCharacter,
      - Added selection of Excel or Text file to uitpInput.R and
        modified other aspects to separate out the delimiter selection
        logic.
      - Default file type is Excel.
      - If a user selects and Excel file and an Excel file is detected,
        all file type and delimiter selections are ignored and the Excel
        file is used and no error or warning is given.
  - Improved checkRequiredCols, toCharacter and getDatedFileName
    functions
  - Exported set\_seed. This will be moved into rmsutilityr
  - Removed erroneous toCharacter documentation
  - Added set\_seed
      - Tried unsuccessfuly to use the RNGkind function and the
        sample.kind argument to set.seed, but found neither existed
        prior to R 3.6.
      - Created a R version sensitive version of set\_seed that
        duplicates the pre-R version 3.6 set.seed function. This is only
        useful for creating data structures for testing purposes and
        should not be used to set seeds for large simulations

# nprcmanager 0.5.13 (20190508)

  - Updated unit tests that were using set.seed to use a R version
    sensitive set.seed wrapper.

# nprcmanager 0.5.12 (20190507)

  - Updated nprcmanager.R to add **Pedigree Testing** and **Plotting**
    function lists.

# nprcmanager 0.5.11 (20190430)

  - Changed wording and format above Suspicious Parent table in ErrorTab
  - Removed row label from Suspicious Parent table
  - Updated meeting notes

# nprcmanager 0.5.10 (20190428)

  - Corrected roxygen2 comment “@export” in getAnimalsWithHighKinship().
  - Added unit test for fillGroupMembersWithSexRatio()

# nprcmanager 0.5.09 (20190428)

  - Corrected bug where parents with suspicious dates were not being
    reported.
  - Improved display of parents with suspicious dates by outputing HTML
    table to the ErrorTab.

# nprcmanager 0.5.08 (20190418)

  - Minor rewording of option label on breeding group formation tab

# nprcmanager 0.5.07 (20190408)

  - Rearranged and reformatted breeding group formation tab

# nprcmanager 0.5.06 (20190407)

  - Changed spelling of gu.iter and gu.thresh to guIter and guThresh

# nprcmanager 0.5.05 (20190406)

  - Fixed all but one bug associated with having multiple dynamically
    generated seed animal groups.
  - Added global definition of MAXGROUPS, which is current set as 10 and
    allows up to six seed animal groups.
  - Corrected test\_fillBins, which was erroneously using a current date
    instead of a fixed date for calculating age.

# nprcmanager 0.5.04 (20190225)

  - Adding ability to have up to six seed animal groups.
  - Added conditional appearance of Make Groups action button that is
    dependent on the user having select on of the optional group
    formation workflows.

# nprcmanager 0.5.03 (20190215)

  - Adding new version of breeding group formation UI and related server
    code.

# nprcmanager 0.5.02 (20190103)

  - Added ability to specify sex ratio in increments of 0.5
    (Female/Male) from 0.5 to 10 in increments of 0.5.

# nprcmanager 0.5.01 (20181230)

  - Correction of some bugs in harem creation and provided additional
    unit tests for harem creation to prevent regression.

# nprcmanager 0.5.00 (20181228)

  - First draft with harem group creation working.
      - Fails if more than one potential sire (male and at least of
        minimum age) is in the current group.
      - Fails if there are insufficient males to have one per breeding
        group being formed.
      - Requires the user to provide males in the candidate set that are
        appropriate for breeding as the current code does not check to
        ensure the animals are still alive. This could easily be added.
      - Males are selected for each group randomly at each iteration
        just as are all other members. The only difference betweeen
        animal selection for harems is that sex is part of the selection
        process.
      - This required the creation of a few functions and modification
        of others. Unit tests were updated to reflect changes, but not
        additions. New unit tests are needed.
      - The format of the breeding group creation page must be improved.
      - The changes made and the new unit tests will serve to simplify
        adding the sex ratio criterion to breeding group formation.

# nprcmanager 0.4.23 (20181226)

  - Added code to detect LabKey connection failure and report it on an
    Error tab

# nprcmanager 0.4.22 (20181224)

  - Minor text changes to Input tab. Refactored groupAddAssign function
    to have a function create the return list.

# nprcmanager 0.4.20 (20181222)

  - Refactor of **groupAddAssign** function by extracting much of the
    function into separate functions. One such function,
    **fillGroupMembers** isolates the group formation code to allow
    adding the ability to satisfy sex ratio requirements and harem
    creation.

# nprcmanager 0.4.19 (20181217)

  - All minor interface changes
      - Substituted hovertext for description of minimum parental age
      - Added meeting notes for 20181210 meeting
      - Changed label on button controling reading of pedigree
        information
      - Updated logo
  - Added code of conduct file.
  - Corrected license text

# nprcmanager 0.4.18 (20181210)

  - Added unit test for removing animals added to pedigree because they
    are unknown parents

# nprcmanager 0.4.17 (20181208)

  - Changed error reporting so as not to report as an error the wrong
    sex when animals are added into the pedigree and appear as both a
    sire and dam without an ego record. The error report now indicates
    these are both a sire and a dam. Done 20181208
  - Made a combined logo for Oregon and SNPRC. Have ONPRC on top using
    blue and green. Done 20181208
  - Additional unit tests to cover all of the new functions created to
    handle the PEDSYS and military formated dates (YYYYMMDD) have been
    made. Done 20181112
  - Corrected breeding groups formation, which was including unknown
    animals that had been added as placeholders for unknown parents.
    Done 20181119
  - Hardened LabKey code by trapping a bad base URL in the configuration
    file with a tryCatch function and send a message to the log file.
    This needs to be tested with a working LabKey system.
