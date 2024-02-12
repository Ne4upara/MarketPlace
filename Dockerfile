FROM openjdk:17-jdk-slim

WORKDIR /app

COPY build/libs/marketplace-0.0.1.jar app.jar

EXPOSE 9999

ENV ACTIVE_PROFILES=prod

CMD ["java", "-jar", "app.jar"]
