#FROM openjdk:17-alpine
#
##RUN apk --no-cache add postgresql
#
#COPY ./build/libs/marketplace-0.0.1.jar /app/marketplace-0.0.1.jar
#
#WORKDIR /app
#
#CMD ["java", "-jar", "marketplace-0.0.1.jar"]

FROM ubuntu:latest AS build
RUN apt-get update
RUN apt-get install -y openjdk-17-jdk
COPY . .
RUN chmod +x gradlew
RUN ./gradlew bootJar --no-daemon

FROM openjdk:17-alpine
EXPOSE 80
COPY --from=build /build/libs/marketplace-0.0.1.jar app.jar

ENTRYPOINT ["java", "-jar", "marketplace-0.0.1.jar"]