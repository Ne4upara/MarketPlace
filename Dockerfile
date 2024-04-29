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