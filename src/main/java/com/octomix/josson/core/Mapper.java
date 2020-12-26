package com.octomix.josson.core;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import java.text.DateFormat;

class Mapper extends ObjectMapper {
    Mapper() {
        this.registerModule(new JavaTimeModule());
        this.setDateFormat(DateFormat.getInstance());
    }
}
