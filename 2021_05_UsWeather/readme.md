---
information for US wheater data set
---
found on:  
  https://www.kaggle.com/sobhanmoosavi/us-weather-events
  
with acknowledgements to:  
  https://smoosavi.org/datasets/lstw
  Moosavi, Sobhan, Mohammad Hossein Samavatian, Arnab Nandi, Srinivasan Parthasarathy, and Rajiv Ramnath. “Short and Long-term Pattern Discovery Over Large-Scale Geo-Spatiotemporal Data.” In Proceedings of the 25th ACM SIGKDD International Conference on Knowledge Discovery & Data Mining, ACM, 2019.

licensed under:  
  CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
  
  
Weather event is a spatiotemporal entity, where such an entity is associated with location and time. Following is the description of available weather event types:
  
|  # |    Attribute    |                                                                                                 Description                                                                                                | Nullable |
|:--:|:---------------:|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:--------:|
|  1 |     EventId     | This is the identifier of a record                                                                                                                                                                         |    No    |
|  2 |       Type      | The type of an event; examples are accident and congestion.                                                                                                                                                |    No    |
|  3 |     Severity    | The  severity is a value between 0 and 4, where 0 indicates the least impact  on traffic (i.e., short delay as a result of the event) and 4 indicates  a significant impact on traffic (i.e., long delay). |    No    |
|  4 |       TMC       | Each traffic event has a Traffic Message Channel (TMC) code which provides a more detailed description on type of the event.                                                                               |    No    |
|  5 |   Description   | The natural language description of an event.                                                                                                                                                              |    No    |
|  6 | StartTime (UTC) | The start time of an event in UTC time zone.                                                                                                                                                               |    No    |
|  7 |  EndTime (UTC)  | The end time of an event in UTC time zone.                                                                                                                                                                 |    No    |
|  8 |     TimeZone    | The US-based timezone based on the location of an event (eastern, central, mountain, and pacific).                                                                                                         |    No    |
|  9 |   LocationLat   | The latitude in GPS coordinate.                                                                                                                                                                            |    Yes   |
| 10 |   LocationLng   | The longitude in GPS coordinate.                                                                                                                                                                           |    Yes   |
| 11 |  Distance (mi)  | The length of the road extent affected by the event.                                                                                                                                                       |    Yes   |
| 12 |   AirportCode   | The closest airport station to the location of a traffic event.                                                                                                                                            |    Yes   |
| 13 |      Number     | The street number in address record.                                                                                                                                                                       |    Yes   |
| 14 |      Street     | The street name in address record.                                                                                                                                                                         |    Yes   |
| 15 |       Side      | The relative side of a street (R/L) in address record.                                                                                                                                                     |    Yes   |
| 16 |       City      | The city in address record.                                                                                                                                                                                |    Yes   |
| 17 |      County     | The county in address record.                                                                                                                                                                              |    Yes   |
| 18 |      State      | The state in address record.                                                                                                                                                                               |    Yes   |
| 19 |     ZipCode     | The zipcode in address record.                                                                                                                                                                             |    Yes   |


   Severe-Cold: The case of having extremely low temperature, with temperature below -23.7 degrees of Celsius.  
   Fog: The case where there is low visibility condition as a result of fog or haze.  
   Hail: The case of having solid precipitation including ice pellets and hail.  
   Rain: The case of having rain, ranging from light to heavy.  
   Snow: The case of having snow, ranging from light to heavy.  
   Storm: The extremely windy condition, where the wind speed is at least 60 km/h.  
   Other Precipitation: Any other type of precipitation which cannot be assigned to previously described event types.  
