# codama 1.4.0 - 2025.09.08

## Added
AkadoR check added : 
* logbook_activity_sample_control.R : Gives inconsistencies between the sample and the activity in terms of presence
* logbook_anapo_activity_control.R : Gives the inconsistencies between the VMS and the presence of activity
* logbook_anapo_control.R : Gives the inconsistencies activity position and VMS position
* logbook_category_species_forbidden_well_control. R : Gives the inconsistencies between the weight categories and the species in the well
* logbook_distribution_control.R : Gives the inconsistencies between the weights of small and big sample fish and the sum of the small and big weights in the associated well
* logbook_eez_control.R : Gives the inconsistencies between the fishing area declared and calculated for the activity
* logbook_fishing_context_control.R : Gives the inconsistencies between the school type and the association
* logbook_fishing_time_control.R : Gives the inconsistencies between the sum of the fishing times indicated for the route and the one indicated for the trip 
* logbook_harbour_control.R : Gives the inconsistencies between the harbour of landing of the previous trip and the harbour of departure of the current trip
* logbook_landing_control.R : Gives the inconsistencies between the total landed weight for canneries and local market in the trip and vessel capacity link to trip
* logbook_landing_total_weight_control.R : Gives the inconsistencies between the total weight landed during the trip for the canneries and the sum of the weights of each landing for the canneries linked to the trip
* logbook_ldlf_control.R : Gives the inconsistencies between the sample measurement types and species or weight values
* logbook_length_class_control.R : Gives the inconsistencies between size class of the samples depending on the species and measurement type and the valid threshold
* logbook_little_big_control.R : Gives the inconsistencies between the percentage of little and big fish sampled
* logbook_measure_control.R : Gives the inconsistencies between the total number of individuals measured per sample and the sum of individuals per sample, species and size class
* logbook_operation_control.R : Gives the inconsistencies between fishing success status, vessel activity, type of school or weight caught
* logbook_position_control.R : Gives the inconsistencies between the ocean declared for the trip and the position of the activity
* logbook_raising_factor_control.R : Gives the inconsistencies between RF1 and threshold values
* logbook_sample_harbour_control.R : Gives inconsistencies between the presence of a sample and the absence of a harbour of landing
* logbook_sample_without_measure_control.R : Gives inconsistencies between the sample and the measurement in terms of presence
* logbook_sample_without_species_control.R : Gives inconsistencies between the sample and the species sampled in terms of presence
* logbook_sea_time_control.R : Gives the inconsistencies between the sum of the sea times indicated for the route and the one indicated for the trip
* logbook_species_control.R : Gives the inconsistencies between species sampled and species authorized
* logbook_super_sample_number_control.R : Gives the inconsistencies between the sample and the subsample number
* logbook_temperature_control.R : Gives the inconsistencies between activity sea surface temperature and valid threshold
* logbook_temporal_limit_control.R : Gives the inconsistencies between trip start and end date and the dates of activity
* logbook_time_route_control.R : Gives the inconsistencies between the fishing times or sea times indicated for the route and activities carried out
* logbook_trip_activity_control.R : Gives the inconsistencies between the trip and the associated activities in terms of presence
* logbook_weight_control.R : Gives the inconsistencies between the sum of the weight indicated for catches and the one indicated for the activity
* logbook_weight_sample_control.R : Gives the inconsistencies between the sample weight (m10 and p10) and the global sample weight
* logbook_weighting_control.R : Gives the inconsistencies between the sample weighting and sample weight or landed weight
* logbook_weighting_sample_control.R : Gives the inconsistencies between the sample weighting and catch weight
* logbook_well_number_control.R : Gives the inconsistencies between sample well number and associated trip well numbers

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

