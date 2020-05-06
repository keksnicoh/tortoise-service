# turtle-service

As we host tortoises on our balcony we must ensure that the thermal properties of their home classify habitable.
Sensor data, [originating from a raspberry pi][1], allow monitoring of the terrarium.
Manual or automatic actions can be triggered to control the environment (turn on/off light bulp, ...)

This is a Haskell practice project.

## environment

All environment variables are prefixed with `TORTOISE_SERVICE`, so `*_A` maps to `TORTOISE_SERVICE_A`

| ENV                     | Description                            | type    | required |
|:------------------------|:---------------------------------------|:--------|:---------|
| `*_PSQL`                | psql connnection string                | string  | __YES__  |
| `*_ASSETS_PATH`         | path at which assets are located       | string  | __YES__  |
| `*_OPEN_WEATHER_MAP_API`| open weather map forecast endpoint uri | string  | __YES__  |
| `*_APPLICATION_MODE`    | development, staging, production       | string  | __YES__  |
| `*_PORT`                | port, default `8081`                   | int     | __NO__   |
| `*_PSQL_SPECS`          | specs psql connection string           | string  | __NO__   |

example (`local-service.sh`)

```bash
export TORTOISE_SERVICE_APPLICATION_MODE="development"
export TORTOISE_SERVICE_PSQL="host='localhost' user='postgres' password='docker' dbname='test'"
export TORTOISE_SERVICE_ASSETS_PATH="./assets"
export TORTOISE_SERVICE_OPEN_WEATHER_MAP_API="https://api.openweathermap.org/data/2.5/forecast?appid=<key>&lat=54&lon=10"
export TORTOISE_SERVICE_PORT=1337
export TORTOISE_SERVICE_PSQL_SPECS="host='localhost' user='postgres' password='docker' dbname='test_hspec'"
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

[1]: https://github.com/keksnicoh/tortoise-client