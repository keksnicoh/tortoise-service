CREATE TABLE "public"."status" (
    "status_id" uuid NOT NULL,
    "temperature" float4,
    "humidity" float4,
    "created" timestamptz NOT NULL,
    PRIMARY KEY ("status_id")
);