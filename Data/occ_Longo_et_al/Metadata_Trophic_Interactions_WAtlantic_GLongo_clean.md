# Dataset for camera data  
## data/Data_Trophic_Interactions_WAtlantic_GLongo_english.csv  
### Variable descriptions  
*movie_filename_code:* This is a unique identifier for the video; it is a combination of the location and site names, date, depth, diver name, and actual video file name stored on hard drive;  
*diver:* Name of diver who shot the video;  
*location:* Name of location;  
*site:* Name of the site (nested in location);  
*date:* Date when video was shot;  
*total_time:* Full video time (in minutes and seconds);  
*effective_time:* Time (in minutes and seconds) in which observations took place;  
*depth_m:* Depth where video was shot (in metres);  
*temperature_celsius* Water temperature in Celsius recorded in situ;  
*time_of_day:* Time of day when video was shot. Incomplete column, should be ignored;  
*beginning_time:* Exact minute and second of video when observation began to be annotated;  
*take:* Take A are the first 5 min, Take B are the second 5 min;  
*family:* Fish taxonomical family;  
*functional_group:* functional group according to diet: "scrp" = herbivore, scraper; "minv" = mobile invertebrate feeder; "dpla" = diurnal planktivore; "ther" = herbivore, small territorial; "fbrow" = herbivore, fine browser; "excv" = herbivore, excavator; "rbrow" = herbivore, rough browser; "sinv" = sessile invertebrate feeder; "omni" = omnivore; "carn" = carnivore;  
*species_code:* Code of species name. Three first letters are genus, latter three are specific name. Example: abu_sax = Abudefduf saxatilis;  
*body_size_cm:* Total body length (cm), 1-cm scale;  
*constant_a:* Length-weight size-independent constant (1 / cm^-coefficient_b). Formula is W = a * L^b;  
*coefficient_b:* Dimensionless length-weight exponent;  
*individual_body_mass_g:* Individual body mass following length-weight formula: individual_body_mass_g = constant_a * body_size_cm ^ coefficient_b;  
*number_of_bites:* Number of bites within the observation window; when activity includes "feed" but bites are missing (or are 0), it is probably a planktivorous fish that fed on plankton, but number of bites could not be discerned;  
*bites_kg_index:* number_of_bites x individual_body_mass_g;  
*activity:* Type of fish activity recorded;  
*substratum:* Type of substratum where fish activity was recorded;  
*rock_inclination_angle:* Rock inclination, from 1 to 4, corresponding respectively from 90˚ (fully vertical, 1) to 0˚ (fully horizontal, 4);  
*ending_time:* Exact minute and second of video when observation ended;  
*observation_time:* Total observation time (in minutes and seconds);  
*individual_ID:* Individual ID within the plot. Annotated for individuals that are easy to identify, based on body marks and behaviour. Common for small home-range species;  
