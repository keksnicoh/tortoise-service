# Changelog for tortoise-service

- **0.2.1** OpenEnv
  - migrated project to use OpenEnv

- **0.2.0** House Automation & Parametrized Environment
  - Emergency Mode if temperature is out of bounds (`Automation.HouseFSM`)
  - For bound temperature, simple house controlling based on two temperature intervals is implemented (`Automation.Free`)
  - Parametrized environment so that effects may embed into application monad
  - SwaggerUI in development mode: `/swagger-ui/index.html`
  - Improved websockets API protocol

- **0.1.0** Initial Feature Set
  - Record basic sensor data
  - House Monitoring
  - Persist webcam image
  - TimeSeries of sensor data
  - Manual light switch controll
  - Websockets API for server <-> client communication

## Unreleased changes
