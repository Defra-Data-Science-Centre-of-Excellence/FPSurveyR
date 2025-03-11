# FPSurveyR 1.1.3

* Add conditions for dealing with missing `fps_gor` factors "East of England" and "South East including London" in `fps_format_factors`.
* Add printing of warning messages if NA levels present for any factor in `fps_format_factors`.

# FPSurveyR 1.1.2

* Add validation to account for missing `.data` in narrative and chart functions.

# FPSurveyR 1.1.1

* Patch to update `round_percent` validation to prevent running on values less than -1 instead of less than 0. 

# FPSurveyR 1.1.0

* Addition of `fps_data_to_binary` function to compliment overhaul of pipeline's validation and formatting scripts.
* Update existing functions to account for new "_v" format response columns.
* Update existing functions to work with new lists in pipeline (e.g. questions - row number lookup in dataset script).
* Allow running of chart and narrative functions when questions are missing (outputs markdown-formatted message instead).

# FPSurveyR 1.0.0

* Initial release of FPSurveyR to Github.
