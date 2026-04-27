# Data Dictionary - Mobile Crisis Response Services Analysis

## Available raw data

- SPD Call for Services, 2015-2025 : .xlsx document, one sheet per year
- SPD Responding Units, 2015-2025 : .xlsx document, one sheet per year
- SPD Call Outcome, 2015-2025 : .xlsx doxument, key codes and one sheet per year

- MCS LC data, 2024/08/18 - 2025/12/17 : .xlsx document, one sheet

- Eugene CAD data, 2015-2025 : one .csv per year

## Processed Data

### SPD data

This dataset contains detailed records of calls for service for Springfield Police Department from 2015 to 2025, including temporal management of calls, the nature of the interventions, and the mobile resources deployed.

There are 14 columns and 567,655 rows.

| Variable               | Type      | Example Value         | Description                                                                    |
|------------------------|-----------|-----------------------|--------------------------------------------------------------------------------|
| `timestamp`            | datetime  | 2015-01-01 01:16:21   | Date and time when the incident was initially recorded.                        |
| `incident_id`          | double    | 15000074              | Unique identifier for each incident.                                           |
| `agency`               | factor    | SPD                   | Agency responsible for the intervention.                                       |
| `city`                 | factor    | Springfield           | City.                                                                          |
| `nature_code`          | factor    | ALMAUD                | Coded category for the type of call received.                                  |
| `nature_final`         | factor    | AUDIBLE ALARM         | Final classification of the incident after assessment.                         |
| `close_code`           | factor    | BLDS                  | Coded reason or outcome for closing the incident.                              |
| `close_code_definition`| factor    | Building Check Secure | Full description of the incident closure status.                               |
| `priority`             | factor    | 3                     | Numerical priority level assigned to the call.                                 |
| `prime_unit`           | character | 1S18                  | Primary unit assigned to respond to the incident.                              |
| `units`                | character | 1S18, 1S21, K94       | List of all units dispatched to the scene.                                     |
| `dispatch_time`        | time      | 01:18:00              | Time at which units were dispatched.                                           |
| `arrival_time`         | time      | 01:21:00              | Time at which the first unit arrived at the scene.                             |
| `clear_time`           | time      | 01:32:00              | Time at which the incident was marked as completed.                            |
| `call_source`          | factor    | PHONE                 | Origin or method of the incoming call.                                         |



### Eugene CAD data

This dataset contains detailed records of calls for service for Eugene from 2015 to 2025, including temporal management of calls, the nature of the interventions, and the mobile resources deployed.

There are 17 columns and 1,446,014 rows.

| Variable              | Type      | Example Value         | Description                                                                    |
|-----------------------|-----------|-----------------------|--------------------------------------------------------------------------------|
| `timestamp`           | datetime  | 2015-01-01 00:00:00   | Date and time when the call to report an incident was initially recorded.      |
| `incident_id`         | double    | 15000001              | Unique identifier for each incident.                                           |
| `agency`              | factor    | EPD                   | Agency responsible for the intervention (e.g., Eugene Police Department).      |
| `service`             | factor    | LAW                   | Type of service mobilized (e.g., Law Enforcement, Fire, Other).                |
| `call_source`         | factor    | SELF                  | Origin of the call (e.g., Self-initiated, Phone, W911).                        |
| `nature`              | factor    | PERSON STOP           | Nature or reported motive of the incident.                                     |
| `close_code`          | factor    | ASST                  | Abbreviated closing code indicating the outcome of the intervention.           |
| `closed_as`           | factor    | ASSISTED              | Full description of how the incident was resolved.                             |
| `priority`            | factor    | 6                     | Priority level assigned to the incident ranging from 1 (highest) to 9 (lowest).|
| `dispatch_time`       | time      | 00:00:00              | Time when units were dispatched to the incident location.                      |
| `arrival_time`        | time      | 00:00:00              | Time when the first unit arrived on the scene.                                 |
| `clear_time`          | time      | 00:03:37              | Time when the incident was marked as cleared or completed.                     |
| `dispatch`            | factor    | 1                     | Binary indicator specifying whether a unit was dispatched.                     |
| `arrival`             | factor    | 1                     | Binary indicator specifying whether a unit arrived on the scene.               |
| `prime_unit`          | factor    | _5E48                 | Identifier of the primary unit assigned to the incident.                       | 
| `nb_units_dispatched` | double    | 1                     | Total number of units dispatched to the intervention.                          |
| `nb_units_arrived`    | double    | 1                     | Total number of units that successfully reached the scene.                     |


### Mobile Crisis Services - Lane County data

This dataset tracks crisis intervention activities from MCS LC, focusing on response timelines, and clinical outcomes for mobile crisis units.

| Variable                     | Type      | Example Value         | Description                                                                 |
|------------------------------|-----------|-----------------------|-----------------------------------------------------------------------------|
| `timestamp`                  | datetime  | 2025-12-17 02:57:00   | Initial date and time the crisis call was logged.                           |
| `incident_id`                | double    | 13209                 | Unique identifier for the crisis incident.                                  |
| `agency`                     | factor    | MCS LC                | The specific Mobile Crisis Service agency responding to the call.           |
| `city`                       | factor    | Springfield           | The city or municipality where the incident occurred.                       |
| `call_type`                  | factor    | Agitation             | The primary clinical nature or behavioral reason for the call.              |
| `call_outcome`               | factor    | Remained in community | The final disposition or result of the intervention.                        |
| `dispatch_time`              | datetime  | 2025-12-17 03:14:00   | Time when the mobile crisis team was officially dispatched.                 |
| `client_engagement_time`     | datetime  | 2025-12-17 03:38:00   | Time when the team made direct clinical contact with the client.            |
| `arrival_time`               | datetime  | 2025-12-17 03:35:00   | Time when the mobile crisis unit arrived at the scene.                      |
| `clear_time`                 | datetime  | 2025-12-17 03:48:00   | Time when the unit completed the intervention and became available again.   |
| `dispatch_status`            | factor    | Engaged               | Status of the dispatch (e.g., Engaged, Refused).                            |
| `minutes_request_dispatch`   | double    | 17                    | Duration in minutes between the initial request and dispatch.               |
| `minutes_dispatch_arrival`   | double    | 21                    | Duration in minutes between dispatch and arrival on scene.                  |
| `minutes_arrival_engagement` | double    | 3                     | Duration in minutes between arrival and start of client engagement.     |
| `minutes_arrival_departure`  | double    | 13                    | Total time spent on scene in minutes (arrival to departure).                |



## Abbreviations

- SPD = Springfield Police Department
- CAD = Computer-Aided Dispatch
- MCS LC = Mobile Crisis Services Lane County

