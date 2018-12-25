NEWS
================
R. Mark Sharp
12/24/2018

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
