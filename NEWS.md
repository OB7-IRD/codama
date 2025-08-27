# codama 1.3.2- 2025.08.27

## Changed
* r_type_checking.R : Add Date to choices for argument type

# codama 1.3.1- 2025.06.06

## Changed
* r_type_checking.R : Add PqConnection to choices for argument type

# codama 1.3.0- 2025.05.12

## Changed
* sample_with_no_fate_control.R, sex_control.R, fate_by_species_group_control.R, weight_null_control.R : Add parametre export_table  
* measure_type_correction.R, sample_with_no_fate_correction.R : Remove condition on database name + Add of the case where there is no sample
* weight_null_control.R : Change filter for catch and sample (preservation of tuna destined for the cannery)
* fate_by_species_group_control.R : Add percentage of automatic corrections that can be made
* size_distribution_and_outliers_control.R : Invert colors in the plot

## Added
* observe_sex.sql : Sex id, code and label mapping table
* sex_correction.R : Corrects the samples with an incorrect sex
* weight_null_correction.R : Corrects the samples with weight = 0
* round_size_control.R : Controls the accuracy (decimals) of size samples
* round_size_correction.R : Corrects unrounded size samples to the inferior centimeter (or half centimeter for PD1)
* fate_by_species_group_correction.R : Corrects erroneous fate in catch and samples for species according to the species group

# codama 1.2.0- 2025.02.20

## Changed
* check_species_catch_ocean.R, marine_area_overlay_control.R : Remove importFrom 
* measure_type_correction.R, sample_with_no_fate_correction.R : Add authors and note to function descriptions

## Added
* sex_control function : Check the coherence of the sex of a sample according to the possibility of sexing this species
* observed_system_vs_species_caught_control function : Identifies in the Observer data any inconsistencies between observed systems and species caught
* observed_systems_vs_species_caught sql
* fate_unknown_control function : Identifies in the observer data cases where fate code 9 (other) or 11 (discarded unknown) were used
* fate_by_species_group_control function : Identifies in the observer data all the species with an inconsistent fate according to their species group
* weight_null_control function : Identifies in the observer data all individual weights = 0 or NA
* reason_for_discard_unknown_control function : Identifies in the observer data cases where the reason for discard is 99 or NA

## Delete
* fate_code_9_control function : Identifies in the Observer data all the fate code 9 and the comment associated

# codama 1.1.1- 2025.02.06

## Changed
* all_species_control.R, fate_code_9_control.R, measure_type_control.R, more_samples_than_count.R, sample_with_no_fate_control.R, size_distribution_and_outliers_control.R : Add authors and note to function descriptions

## Added
* Citation.cff

# codama 1.1.0- 2024.11.26

## Added
* Add check_raising_factor_inspector function

# codama 1.0.0- 2024.14.26

## Added
* Start of version number

