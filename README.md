# tortoise-service

As we host tortoises on our balcony we must ensure that the thermal properties of their home classify habitable.
Sensor data, [originating from a raspberry pi][1], allow monitoring of the terrarium.
Manual or automatic actions can be triggered to control the environment (turn on/off light bulp, ...).
[tortoise-ui][2] implements a simple standalone vuejs user interface to this service.

This is a Haskell practice project.

## environment

All environment variables are prefixed with `TORTOISE_SERVICE`, so `*_A` maps to `TORTOISE_SERVICE_A`

| ENV                      | Description                                                    | type   | required |
|:-------------------------|:---------------------------------------------------------------|:-------|:---------|
| `*_PSQL`                 | psql connnection string                                        | string | __YES__  |
| `*_ASSETS_PATH`          | path at which assets are located                               | string | __YES__  |
| `*_OPEN_WEATHER_MAP_API` | open weather map forecast endpoint uri                         | string | __YES__  |
| `*_APPLICATION_MODE`     | development, staging, production                               | string | __YES__  |
| `*_PORT`                 | port, default `8081`                                           | int    | __NO__   |
| `*_PSQL_SPECS`           | specs psql connection string                                   | string | __NO__   |
| `*_FSM_SENSOR_DELAY`     | delay in seconds to read HouseState sensor, default `60`.      | int    | __NO__   |
| `*_FSM_MIN_TEMPERATURE`  | a house temperature below triggers emergency, default `15`     | float  | __NO__   |
| `*_FSM_MAX_TEMPERATURE`  | a house temperature above triggers emergency, default `35`     | float  | __NO__   |
| `*_FSM_RETRY`            | number of retries if sensor couold not be read, default `5`    | int    | __NO__   |
| `*_FSM_EMERGENCY_DELAY`  | duration in seconds of emergency state, default `900`          | int    | __NO__   |
| `*_FSM_SC_L1_TLOW`       | temperature below light1 is turned back on, default `16`       | float  | __NO__   |
| `*_FSM_SC_L1_THIGH`      | temperature below light1 is turned back off, default `25`      | float  | __NO__   |
| `*_FSM_SC_L2_TLOW`       | temperature below light2 is turned back on, default `20`       | float  | __NO__   |
| `*_FSM_SC_L2_THIGH`      | temperature below light1 is turned back off, default `34`      | float  | __NO__   |
| `*_FSM_SC_LOCK_DURATION` | duration to lock a light after automatic control, default `600`| int    | __NO__   |

example (`local-service.sh`)

```bash
export TORTOISE_SERVICE_APPLICATION_MODE="development"
export TORTOISE_SERVICE_PSQL="host='localhost' user='postgres' password='docker' dbname='test'"
export TORTOISE_SERVICE_ASSETS_PATH="./assets"
export TORTOISE_SERVICE_OPEN_WEATHER_MAP_API="https://api.openweathermap.org/data/2.5/forecast?appid=<key>&lat=54&lon=10"
export TORTOISE_SERVICE_PORT=1337
export TORTOISE_SERVICE_PSQL_SPECS="host='localhost' user='postgres' password='docker' dbname='test_hspec'"

export TORTOISE_SERVICE_FSM_EMERGENCY_DELAY=900
export TORTOISE_SERVICE_FSM_SENSOR_DELAY=60
export TORTOISE_SERVICE_FSM_MIN_TEMPERATURE=15
export TORTOISE_SERVICE_FSM_MAX_TEMPERATURE=35
export TORTOISE_SERVICE_FSM_RETRY=5

export TORTOISE_SERVICE_FSM_SC_L1_TLOW=16
export TORTOISE_SERVICE_FSM_SC_L1_THIGH=25
export TORTOISE_SERVICE_FSM_SC_L2_TLOW=20
export TORTOISE_SERVICE_FSM_SC_L2_THIGH=34
export TORTOISE_SERVICE_FSM_SC_LOCK_DURATION=600
```

## features

- receive sensor data
- provide monitoring
- weather forecast from external api
- websockets to send messages to a client
- webcam endpoints to send or request an image
- sensor data timeseries for different periods and resolutions
- light switch control

## upcoming / todo

- documentation of the raspberry pi project
- implement missing specs
- webcam endpoint swagger docs
- control of light switches from differential, intergral and dynamical parameters

## setup

For local execution, create a psql db via docker

```bash 
docker run --rm --name pg-docker \
  -e POSTGRES_PASSWORD=docker    \
  -d -p 5432:5432                \
  -v <path-to-mnt>               \
  postgres
```

Open psql cli and create table by executing `sql/db.dql`

Execute tests

```
source local-service.sh
stack test
```

Run application 

```
source local-service.sh
stack run
```

## tortoise-ui

finally, the enduser experience looks like this (vuejs app).
[tortoise-ui repo][2].

![screenshot of tortoise-app](https://github.com/keksnicoh/tortoise-service/blob/master/resources/tortoise-app.png)

## tortoise-client

[raspberry pi project repo][1]

![screenshot of raspberry pi](https://github.com/keksnicoh/tortoise-service/blob/master/resources/pi.png)

[1]: https://github.com/keksnicoh/tortoise-client
[2]: https://github.com/keksnicoh/tortoise-ui
