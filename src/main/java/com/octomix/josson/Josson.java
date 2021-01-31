package com.octomix.josson;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;

import static com.octomix.josson.JossonCore.*;

public class Josson {

    private JsonNode node;

    private Josson(JsonNode node) {
        this.node = node;
    }

    public static Josson create() {
        return new Josson(createObjectNode());
    }

    public static Josson createArray() {
        return new Josson(createArrayNode());
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
        return new Josson(readJsonNode(object));
    }

    public static Josson fromJsonString(String json) throws IllegalArgumentException, JsonProcessingException {
        if (json == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        return new Josson(readJsonNode(json));
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

    public void setNode(JsonNode node) throws IllegalArgumentException {
        if (node == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        this.node = node;
    }

    public void setJsonString(String json) throws IllegalArgumentException, JsonProcessingException {
        if (json == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        node = readJsonNode(json);
    }

    public <T> T convertValue() {
        return convertValue(node);
    }

    public <E extends Enum<E>> Josson put(String key, Enum<E> value) {
        ((ObjectNode) node).put(key, value == null ? null : value.name());
        return this;
    }

    public Josson put(String key, String value) {
        ((ObjectNode) node).put(key, value);
        return this;
    }

    public Josson put(String key, Long value) {
        ((ObjectNode) node).put(key, value);
        return this;
    }

    public Josson put(String key, Integer value) {
        ((ObjectNode) node).put(key, value);
        return this;
    }

    public Josson put(String key, Double value) {
        ((ObjectNode) node).put(key, value);
        return this;
    }

    public Josson put(String key, Boolean value) {
        ((ObjectNode) node).put(key, value);
        return this;
    }

    public Josson put(String key, LocalDateTime value) {
        ((ObjectNode) node).put(key, value.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
        return this;
    }

    public Josson put(String key, JsonNode value) {
        ((ObjectNode) node).putPOJO(key, value);
        return this;
    }

    public String jsonString() {
        return node.toString();
    }

    public String jsonPretty() {
        return node.toPrettyString();
    }

    public JsonNode getNode() {
        return node;
    }

    public JsonNode getNode(int index) {
        return node.isArray() ? node.get(index) : null;
    }

    public JsonNode getNode(String path) {
        return getNode(node, path);
    }

    public JsonNode getNode(int index, String path) {
        return node.isArray() ? getNode(node.get(index), path) : null;
    }

    public ArrayNode getArrayNode(String path) {
        JsonNode node = getNode(path);
        return node != null && node.isArray() ? (ArrayNode) node : null;
    }

    public int getArraySize() {
        return node.isArray() ? node.size() : -1;
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

    public static ObjectNode createObjectNode() {
        return MAPPER.createObjectNode();
    }

    public static ArrayNode createArrayNode() {
        return MAPPER.createArrayNode();
    }

    public static JsonNode readJsonNode(Object object) {
        return MAPPER.valueToTree(object);
    }

    public static ObjectNode readObjectNode(Object object) {
        JsonNode node = MAPPER.valueToTree(object);
        if (node.isObject()) {
            return (ObjectNode) node;
        }
        throw new IllegalArgumentException("The provided object is not an object node");
    }

    public static ArrayNode readArrayNode(Object object) {
        JsonNode node = MAPPER.valueToTree(object);
        if (node.isArray()) {
            return (ArrayNode) node;
        }
        throw new IllegalArgumentException("The provided object is not an array node");
    }

    public static JsonNode readJsonNode(String json) throws JsonProcessingException {
        if (json == null) {
            return null;
        }
        return MAPPER.readTree(json);
    }

    public static <T> T readValue(String json, Class<T> valueType) throws JsonProcessingException {
        return MAPPER.readValue(json, valueType);
    }

    public static <T> T readValue(File file, Class<T> valueType) throws IOException {
        return MAPPER.readValue(file, valueType);
    }

    public static <T> T convertValue(JsonNode node) {
        return MAPPER.convertValue(node, new TypeReference<T>(){});
    }

    public static String toJsonString(Object object) {
        try {
            return MAPPER.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            return MAPPER.valueToTree(object).toString();
        }
    }

    public static String toJsonPretty(Object object) {
        try {
            return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(object);
        } catch (JsonProcessingException e) {
            return MAPPER.valueToTree(object).toPrettyString();
        }
    }

    public static JsonNode getNode(JsonNode node, String path) {
        if (StringUtils.isBlank(path)) {
            return node;
        }
        List<String> keys = new ArrayList<>();
        Matcher m = DECOMPOSE_PATH.matcher(path);
        while (m.find()) {
            if (!StringUtils.isBlank(m.group(0))) {
                keys.add(m.group(0));
            }
        }
        if (keys.isEmpty()) {
            return node;
        }
        try {
            node = toValueNode(keys.get(0).trim());
            keys.remove(0);
        } catch (NumberFormatException e) {
            // continue
        }
        return getNodeByKeys(node, keys);
    }
}
