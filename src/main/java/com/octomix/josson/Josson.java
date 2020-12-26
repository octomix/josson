package com.octomix.josson;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.core.JossonCore;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

public class Josson {

    private JsonNode jsonNode;

    private Josson(JsonNode jsonNode) {
        this.jsonNode = jsonNode;
    }

    public static Josson create() {
        return new Josson(JossonCore.createObjectNode());
    }

    public static Josson createArray() {
        return new Josson(JossonCore.createArrayNode());
    }

    public static Josson create(JsonNode node) throws IllegalArgumentException {
        if (node == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        return new Josson(node);
    }

    public static Josson from(Object object) throws IllegalArgumentException {
        if (object == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        return new Josson(JossonCore.readJsonNode(object));
    }

    public static Josson fromJsonString(String json) throws IllegalArgumentException, JsonProcessingException {
        if (json == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        return new Josson(JossonCore.readJsonNode(json));
    }

    public static Josson fromText(String v) {
        return new Josson(TextNode.valueOf(v));
    }

    public static Josson fromInt(int i) {
        return new Josson(IntNode.valueOf(i));
    }

    public static Josson fromDouble(double v) {
        return new Josson(DoubleNode.valueOf(v));
    }

    public static Josson fromBoolean(boolean b) {
        return new Josson(BooleanNode.valueOf(b));
    }

    public JsonNode getJsonNode() {
        return jsonNode;
    }

    public void setJsonNode(JsonNode jsonNode) throws IllegalArgumentException {
        if (jsonNode == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        this.jsonNode = jsonNode;
    }

    public void setJsonString(String json) throws IllegalArgumentException, JsonProcessingException {
        if (json == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        jsonNode = JossonCore.readJsonNode(json);
    }

    public <T> T convertValue() {
        return JossonCore.convertValue(jsonNode);
    }

    public <E extends Enum<E>> Josson put(String key, Enum<E> value) {
        ((ObjectNode) jsonNode).put(key, value == null ? null : value.name());
        return this;
    }

    public Josson put(String key, String value) {
        ((ObjectNode) jsonNode).put(key, value);
        return this;
    }

    public Josson put(String key, Long value) {
        ((ObjectNode) jsonNode).put(key, value);
        return this;
    }

    public Josson put(String key, Integer value) {
        ((ObjectNode) jsonNode).put(key, value);
        return this;
    }

    public Josson put(String key, Double value) {
        ((ObjectNode) jsonNode).put(key, value);
        return this;
    }

    public Josson put(String key, Boolean value) {
        ((ObjectNode) jsonNode).put(key, value);
        return this;
    }

    public Josson put(String key, LocalDateTime value) {
        ((ObjectNode) jsonNode).put(key, value.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
        return this;
    }

    public Josson put(String key, JsonNode value) {
        ((ObjectNode) jsonNode).putPOJO(key, value);
        return this;
    }

    public String jsonString() {
        return jsonNode.toString();
    }

    public String jsonPretty() {
        return jsonNode.toPrettyString();
    }

    public JsonNode getNode(String path) {
        return JossonCore.getNode(jsonNode, path);
    }

    public JsonNode getNode(int index, String path) {
        return jsonNode.isArray() ? JossonCore.getNode(jsonNode.get(index), path) : null;
    }

    public ArrayNode getArrayNode(String path) {
        JsonNode node = getNode(path);
        return node != null && node.isArray() ? (ArrayNode) node : null;
    }

    public int getArraySize() {
        return jsonNode.isArray() ? jsonNode.size() : -1;
    }

    public int getArraySize(String path) {
        ArrayNode node = getArrayNode(path);
        return node == null ? -1 : node.size();
    }

    public JsonNode getValueNode(String path) {
        JsonNode node = getNode(path);
        return node != null && node.isValueNode() ? node : null;
    }

    public JsonNode getRequiredValueNode(String path) throws Exception {
        JsonNode node = getNode(path);
        if (node == null) {
            throw new Exception("Missing path: " + path);
        }
        if (!node.isValueNode()) {
            throw new Exception("This path is not a value node: " + path);
        }
        return node;
    }

    public String getString(String path) {
        JsonNode node = getNode(path);
        return node == null || node.isNull() ? null : node.isValueNode() ? node.asText() : node.toString();
    }

    public String getRequiredString(String path) throws Exception {
        return getRequiredValueNode(path).asText();
    }

    public Long getLong(String path) {
        JsonNode node = getValueNode(path);
        return node == null || node.isNull() ? null : node.asLong();
    }

    public Long getRequiredLong(String path) throws Exception {
        return getRequiredValueNode(path).asLong();
    }

    public Integer getInteger(String path) {
        JsonNode node = getValueNode(path);
        return node == null || node.isNull() ? null : node.asInt();
    }

    public Integer getRequiredInteger(String path) throws Exception {
        return getRequiredValueNode(path).asInt();
    }

    public Double getDouble(String path) {
        JsonNode node = getValueNode(path);
        return node == null || node.isNull() ? null : node.asDouble();
    }

    public Double getRequiredDouble(String path) throws Exception {
        return getRequiredValueNode(path).asDouble();
    }

    public Boolean getBoolean(String path) {
        JsonNode node = getValueNode(path);
        return node == null || node.isNull() ? null : node.asBoolean();
    }

    public Boolean getRequiredBoolean(String path) throws Exception {
        return getRequiredValueNode(path).asBoolean();
    }

    public LocalDateTime getIsoLocalDateTime(String path) {
        JsonNode node = getValueNode(path);
        return node == null || node.isNull() ? null : LocalDateTime.parse(node.asText());
    }

    public LocalDateTime getRequiredIsoLocalDateTime(String path) throws Exception {
        return LocalDateTime.parse(getRequiredValueNode(path).asText());
    }

    public LocalDate getIsoLocalDate(String path) {
        JsonNode node = getValueNode(path);
        return node == null || node.isNull() ? null : LocalDate.parse(node.asText());
    }

    public LocalDate getRequiredIsoLocalDate(String path) throws Exception {
        return LocalDate.parse(getRequiredValueNode(path).asText());
    }

    public OffsetDateTime getOffsetDateTime(String path) {
        JsonNode node = getValueNode(path);
        return node == null || node.isNull() ? null : OffsetDateTime.parse(node.asText());
    }

    public OffsetDateTime getRequiredOffsetDateTime(String path) throws Exception {
        return OffsetDateTime.parse(getRequiredValueNode(path).asText());
    }
}
