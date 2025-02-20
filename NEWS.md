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

