# Use the openjdk:17-jdk-slim image as the base image for the production environment
FROM openjdk:17-jdk-slim

# Set the working directory to /app
WORKDIR /app

# Copy the marketplace-0.0.1.jar file to the working directory
COPY build/libs/marketplace-0.0.1.jar app.jar

# Expose port 9999 for the application
EXPOSE 9999

# Set the environment variable ACTIVE_PROFILES to prod
ENV ACTIVE_PROFILES=prod

# Set the default command to run when the container starts
CMD ["java", "-jar", "app.jar"]

# Use the latest version of Ubuntu as the base image for the build environment
FROM ubuntu:latest AS build

# Update the package lists for upgrades and new package installations
RUN apt-get update

# Install the openjdk-17-jdk package
RUN apt-get install -y openjdk-1
