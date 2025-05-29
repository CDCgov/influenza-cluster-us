# Understanding spatiotemporal clustering of seasonal influenza in the United States

[![License](https://img.shields.io/badge/license-Apache_2.0_license-brightgreen)](LICENSE)
[![GitHub stars](https://img.shields.io/github/stars/CDCgov/influenza-cluster-us)](https://github.com/CDCgov/influenza-cluster-us/stargazers)

## Overview

This repository contains the code required to reproduce the results from the study [Chan et al. 2024](https://doi.org/10.1101/2025.05.28.25328505).

For this demonstration, we analyze data from the 2010/2011 to 2023/2024 seasons. This analysis generates results for two main phases: the pre-COVID-19 period (2010/2011–2019/2020) and the post-COVID-19 period (2022/2023–2023/2024), as well as for 14 individual seasons.

In the `R/figure` folder, we have uploaded the main figures as presented in our manuscript. Users can also generate all figures by running the script `R/0_main.R`.

The script `R/0_main.R` contains functions for analyzing surveillance data using parallel processing. The analysis workflow includes *data processing (imputation and smoothing)*, *time series clustering*, and *spatial autocorrelation analysis of peak timing*.

### Data sharing statement

The surveillance data utilized in this study are publicly available through the CDC's FluView Interactive platform <https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html>. All analyses were performed using R version 4.4.0. The R scripts and code supporting the findings of this study are accessible on GitHub at <https://github.com/CDCgov/influenza-cluster-us>.

### Disclaimer

The conclusions, findings, and opinions expressed by authors contributing to this article do not necessarily reflect the official position of the U.S. Department of Health and Human Services, the Public Health Service, the Centers for Disease Control and Prevention, or the authors' affiliated institutions.

### Contact information

**Louis Yat Hin Chan, PhD, MSc**  
CDC Steven M. Teutsch Prevention Effectiveness (PE) Fellow – Analytics and Modeling Track, Class of 2023  
Applied Research and Modeling (ARM) Team  
Epidemiology and Prevention Branch (EPB)  
Influenza division (ID)  
National Center for Immunization and Respiratory Diseases (NCIRD)  
Centers for Disease Control and Prevention (CDC)  

Work address: 1600 Clifton Road, Atlanta, GA 30329  
Work email: <LouisChan@cdc.gov>  

## Public Domain Standard Notice

This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

## License Standard Notice

The repository utilizes code licensed under the terms of the Apache Software
License and therefore is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify it under
the terms of the Apache Software License version 2, or (at your option) any
later version.

This source code in this repository is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the Apache Software License for more details.

You should have received a copy of the Apache Software License along with this
program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

The source code forked from other open source projects will inherit its license.

## Privacy Standard Notice

This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](DISCLAIMER.md)
and [Code of Conduct](code-of-conduct.md).
For more information about CDC's privacy policy, please visit [http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

## Contributing Standard Notice

Anyone is encouraged to contribute to the repository by [forking](https://help.github.com/articles/fork-a-repo)
and submitting a pull request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including but not limited to the Federal Records Act, and may be archived. Learn more at [http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

## Records Management Standard Notice

This repository is not a source of government records, but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).

## Additional Standard Notices

Please refer to [CDC's Template Repository](https://github.com/CDCgov/template) for more information about [contributing to this repository](https://github.com/CDCgov/template/blob/main/CONTRIBUTING.md), [public domain notices and disclaimers](https://github.com/CDCgov/template/blob/main/DISCLAIMER.md), and [code of conduct](https://github.com/CDCgov/template/blob/main/code-of-conduct.md).
