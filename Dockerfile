#FROM openjdk:17-jdk-slim
#
#WORKDIR /app
#
#COPY build/libs/marketplace-0.0.1.jar app.jar
#
#EXPOSE 9999
#
#ENV ACTIVE_PROFILES=prod
#
#CMD ["java", "-jar", "app.jar"]

FROM ubuntu:latest AS build
RUN apt-get update
RUN apt-get install -y openjdk-17-jdk
COPY . .
RUN chmod +x gradlew
RUN ./gradlew bootJar --no-daemon

FROM openjdk:17-alpine
EXPOSE 80
COPY --from=build /build/libs/MarketPlace-0.0.1.jar /app/MarketPlace-0.0.1.jar

ENTRYPOINT ["java", "-jar", "app/MarketPlace-0.0.1.jar"]