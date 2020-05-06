# turtle-service

As we host tortoises on our balcony we must ensure that the thermal properties of their home classify habitable.
Sensor data, [comming from a raspberry pi][1], allows to monitor the terrarium.
Manual or automatic actions can be triggered to control the environment (turn on/off light bulp, ...)

## environment

all environment variables are prefixed with `TORTOISE_SERVICE_`, so `*_A` maps to `TORTOISE_SERVICE_A`

| ENV                     | Description                                                        |
|:------------------------|:-------------------------------------------------------------------|
| `*_PSQL`                | postgresql connnection string. example:<br/> `host='?' user='?' password='?' |
| `*_ASSETS_PATH`         | path at which assets are located. example: <br/> `./assets` |
| `*_OPEN_WEATHER_MAP_API`| open weather map forecast endpoint uri. example: <br/> `https://api.openweathermap.org/data/2.5/forecast?appid=<appid>&lat=54&lon=10` | 
| `*_APPLICATION_MODE`    | application mode (development, stagin, production). example: <br/> `development` |
| `*_PORT`                | application port. example <br/>`1337` |

## features

- receive sensor status
- provide monitoring
- weather forecast from external api
- websockets to send messages to a client
- webcam endpoints to receive an image and request a new image
- time series of sensor data for different periods and resolutions
- light switch control

## upcoming

- upcoming: control of light switches from differential, intergral and dynamical parameters

[1]: https://github.com/keksnicoh/tortoise-client