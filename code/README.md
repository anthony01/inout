# README #

### Extra worktime hours calculation ###

Main files:

"inc_period.R" - defines period and names of the input files

"pre_data.R"   - performs 1-st stage: data aggregation & cleaning

"inout.R"      - performs final calculation & resulting data output (in html format)

"put_html.sh"  - pushes html files to web server

Recipe:

1. Make sure all input files of the required period are present in expected format.

2. Edit "inc_period.R" to adjust period.

3. Run "./pre_data.R"

4. Run "./inout.R"

5. Run "sh ./put_html.sh" to push resulting html files to web server.