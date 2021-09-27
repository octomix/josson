/*
 * Copyright 2020 Octomix Software Technology Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.octomix.josson;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.commons.StringUtils;

import java.io.File;
import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.decomposePaths;

public class Josson {

    private JsonNode jsonNode;

    private Josson(JsonNode node) {
        this.jsonNode = node;
    }

    /**
     * Create a Josson object that contains an empty Jackson ObjectNode.
     *
     * @return The new Josson object
     */
    public static Josson create() {
        return new Josson(createObjectNode());
    }

    /**
     * Create a Josson object that contains an empty Jackson ArrayNode.
     *
     * @return The new Josson object
     */
    public static Josson createArray() {
        return new Josson(createArrayNode());
    }

    /**
     * Create a Josson object with given Jackson JsonNode.
     *
     * @param node the Jackson JsonNode to store
     * @return The new Josson object
     * @throws IllegalArgumentException if {@code node} is null
     */
    public static Josson create(JsonNode node) {
        if (node == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        return new Josson(node);
    }

    /**
     * Create a Josson object with given object that converted to an equivalent JSON Tree representation.
     *
     * @param object the object to convert
     * @return The new Josson object
     * @throws IllegalArgumentException if {@code object} is null
     */
    public static Josson from(Object object) {
        if (object == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        return new Josson(readJsonNode(object));
    }

    /**
     * Create a Josson object with given JSON content string that deserialized to a Jackson JsonNode.
     *
     * @param json the string content for building the JSON tree
     * @return The new Josson object
     * @throws JsonProcessingException if {@code json} is null or the underlying input contains invalid content
     */
    public static Josson fromJsonString(String json) throws JsonProcessingException {
        if (json == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        return new Josson(readJsonNode(json));
    }

    /**
     * Set the Josson content with given Jackson JsonNode.
     *
     * @param node the Jackson JsonNode to store
     * @throws IllegalArgumentException if {@code node} is null
     */
    public void setNode(JsonNode node) {
        if (node == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        this.jsonNode = node;
    }

    /**
     * Set the Josson content with given JSON content string that deserialized to a Jackson JsonNode.
     *
     * @param json the string content for building the JSON tree
     * @throws JsonProcessingException if {@code json} is null or the underlying input contains invalid content
     */
    public void setJsonString(String json) throws JsonProcessingException {
        if (json == null) {
            throw new IllegalArgumentException("Argument cannot be null");
        }
        jsonNode = readJsonNode(json);
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

    /**
     * Simply returns the content.
     *
     * @return The content Jackson JsonNode
     */
    public JsonNode getNode() {
        return jsonNode;
    }

    /**
     * Get an ArrayNode element if the content is an ArrayNode.
     *
     * @param index index of the specific ArrayNode element
     * @return The specific ArrayNode element
     */
    public JsonNode getNode(int index) {
        return jsonNode.isArray() ? jsonNode.get(index) : null;
    }

    /**
     * Query data by Josson query language.
     *
     * @param jossonPath the Josson query path
     * @return The resulting Jackson JsonNode
     * @throws IllegalArgumentException if the query path is invalid
     */
    public JsonNode getNode(String jossonPath) {
        return getNode(jsonNode, jossonPath);
    }

    /**
     * Query data on an element of ArrayNode by Josson query language.
     *
     * @param index index of the specific ArrayNode element
     * @param jossonPath the Josson query path
     * @return The resulting Jackson JsonNode
     * @throws IllegalArgumentException if the query path is invalid
     */
    public JsonNode getNode(int index, String jossonPath) {
        return jsonNode.isArray() ? getNode(jsonNode.get(index), jossonPath) : null;
    }

    /**
     * Query data by Josson query language.
     *
     * @param jossonPath the Josson query path
     * @return A new Josson object with the resulting JsonNode
     */
    public Josson getJosson(String jossonPath) {
        JsonNode node = getNode(jossonPath);
        return node == null ? null : new Josson(node);
    }

    /**
     * Query data on an element of ArrayNode by Josson query language.
     *
     * @param index index of the specific ArrayNode element
     * @param jossonPath the Josson query path
     * @return A new Josson object with the resulting JsonNode
     */
    public Josson getJosson(int index, String jossonPath) {
        JsonNode node = getNode(index, jossonPath);
        return node == null ? null : new Josson(node);
    }

    /**
     * Query data by Josson query language, return result for ArrayNode only.
     *
     * @param jossonPath the Josson query path
     * @return The resulting Jackson ArrayNode. {@code null} if the result is not an ArrayNode.
     * @throws IllegalArgumentException if the query path is invalid
     */
    public ArrayNode getArrayNode(String jossonPath) {
        JsonNode node = getNode(jossonPath);
        return node != null && node.isArray() ? (ArrayNode) node : null;
    }

    /**
     * Query data by Josson query language, return result for ValueNode only.
     *
     * @param jossonPath the Josson query path
     * @return The resulting Jackson ValueNode. {@code null} if the result is not a ValueNode.
     * @throws IllegalArgumentException if the query path is invalid
     */
    public ValueNode getValueNode(String jossonPath) {
        JsonNode node = getNode(jossonPath);
        return node != null && node.isValueNode() ? (ValueNode) node : null;
    }

    /**
     * Query data by Josson query language, return result for ValueNode only.
     *
     * @param jossonPath the Josson query path
     * @return The resulting Jackson ValueNode.
     * @throws IllegalArgumentException if the query path is invalid
     * @throws Exception if the result is not a ValueNode
     */
    public ValueNode getRequiredValueNode(String jossonPath) throws Exception {
        JsonNode node = getNode(jossonPath);
        if (node == null || !node.isValueNode()) {
            throw new Exception("This Josson path cannot evaluate to a value node: " + jossonPath);
        }
        return (ValueNode) node;
    }

    public String getString(String jossonPath) {
        JsonNode node = getNode(jossonPath);
        return node == null || node.isNull() ? null : node.isValueNode() ? node.asText() : node.toString();
    }

    public String getRequiredString(String jossonPath) throws Exception {
        return getRequiredValueNode(jossonPath).asText();
    }

    public Long getLong(String jossonPath) {
        ValueNode node = getValueNode(jossonPath);
        return node == null || node.isNull() ? null : node.asLong();
    }

    public Long getRequiredLong(String jossonPath) throws Exception {
        return getRequiredValueNode(jossonPath).asLong();
    }

    public Integer getInteger(String jossonPath) {
        ValueNode node = getValueNode(jossonPath);
        return node == null || node.isNull() ? null : node.asInt();
    }

    public Integer getRequiredInteger(String jossonPath) throws Exception {
        return getRequiredValueNode(jossonPath).asInt();
    }

    public Double getDouble(String jossonPath) {
        ValueNode node = getValueNode(jossonPath);
        return node == null || node.isNull() ? null : node.asDouble();
    }

    public Double getRequiredDouble(String jossonPath) throws Exception {
        return getRequiredValueNode(jossonPath).asDouble();
    }

    public Boolean getBoolean(String jossonPath) {
        ValueNode node = getValueNode(jossonPath);
        return node == null || node.isNull() ? null : node.asBoolean();
    }

    public Boolean getRequiredBoolean(String jossonPath) throws Exception {
        return getRequiredValueNode(jossonPath).asBoolean();
    }

    public LocalDateTime getIsoLocalDateTime(String jossonPath) {
        ValueNode node = getValueNode(jossonPath);
        return node == null || node.isNull() ? null : LocalDateTime.parse(node.asText());
    }

    public LocalDateTime getRequiredIsoLocalDateTime(String jossonPath) throws Exception {
        return LocalDateTime.parse(getRequiredValueNode(jossonPath).asText());
    }

    public LocalDate getIsoLocalDate(String jossonPath) {
        ValueNode node = getValueNode(jossonPath);
        return node == null || node.isNull() ? null : LocalDate.parse(node.asText());
    }

    public LocalDate getRequiredIsoLocalDate(String jossonPath) throws Exception {
        return LocalDate.parse(getRequiredValueNode(jossonPath).asText());
    }

    public OffsetDateTime getOffsetDateTime(String jossonPath) {
        ValueNode node = getValueNode(jossonPath);
        return node == null || node.isNull() ? null : OffsetDateTime.parse(node.asText());
    }

    public OffsetDateTime getRequiredOffsetDateTime(String jossonPath) throws Exception {
        return OffsetDateTime.parse(getRequiredValueNode(jossonPath).asText());
    }

    /**
     * Convert the content into instance of given value type.
     *
     * @param <T> the specific type of the result
     * @return The generated JSON that converted to the result type
     * @throws IllegalArgumentException if conversion fails due to incompatible type
     */
    public <T> T convertValue() {
        return convertValue(jsonNode);
    }

    /**
     * Serialize the content to a JSON as a string.
     *
     * @return The generated JSON as a string
     */
    public String jsonString() {
        return jsonNode.toString();
    }

    /**
     * Serialize the content using Jackson default pretty-printer.
     *
     * @return The generated JSON as a string in pretty format
     */
    public String jsonPretty() {
        return jsonNode.toPrettyString();
    }

    /**
     * Set serializing inclusion options
     *
     * @param include JsonInclude.Include e.g. NON_NULL
     */
    public static void setSerializationInclusion(JsonInclude.Include include) {
        MAPPER.setSerializationInclusion(include);
    }

    /**
     * Create an empty Jackson ObjectNode.
     *
     * @return The new empty Jackson ObjectNode
     */
    public static ObjectNode createObjectNode() {
        return MAPPER.createObjectNode();
    }

    /**
     * Create an empty Jackson ArrayNode.
     *
     * @return The new empty Jackson ArrayNode
     */
    public static ArrayNode createArrayNode() {
        return MAPPER.createArrayNode();
    }

    /**
     * Convert an object to an equivalent JSON Tree representation.
     *
     * @param object the object to convert
     * @return Root node of the resulting JSON tree
     */
    public static JsonNode readJsonNode(Object object) {
        return MAPPER.valueToTree(object);
    }

    /**
     * Convert an object to a Jackson ObjectNode.
     *
     * @param object the object to convert
     * @return The resulting Jackson ObjectNode
     * @throws IllegalArgumentException if the converted JSON is not a Jackson object
     */
    public static ObjectNode readObjectNode(Object object) {
        JsonNode node = MAPPER.valueToTree(object);
        if (node.isObject()) {
            return (ObjectNode) node;
        }
        throw new IllegalArgumentException("The provided object is not an object node");
    }

    /**
     * Convert an object to a Jackson ArrayNode.
     *
     * @param object the object to convert
     * @return The resulting Jackson ArrayNode
     * @throws IllegalArgumentException if the converted JSON is not a Jackson array
     */
    public static ArrayNode readArrayNode(Object object) {
        JsonNode node = MAPPER.valueToTree(object);
        if (node.isArray()) {
            return (ArrayNode) node;
        }
        throw new IllegalArgumentException("The provided object is not an array node");
    }

    /**
     * Deserialize JSON content string to a Jackson JsonNode.
     *
     * @param json the string content for building the JSON tree
     * @return A JsonNode, if valid JSON content found
     * @throws JsonProcessingException if underlying input contains invalid content
     */
    public static JsonNode readJsonNode(String json) throws JsonProcessingException {
        if (json == null) {
            return null;
        }
        return MAPPER.readTree(json);
    }

    /**
     * Deserialize JSON content string into given Java type.
     *
     * @param json the string content for building the JSON tree
     * @param valueType the result class type
     * @param <T> the specific type of the result
     * @return The deserialized JSON content converted to the result type
     * @throws JsonProcessingException if underlying input contains invalid content
     * @throws JsonMappingException if the input JSON structure does not match structure expected for result type
     */
    public static <T> T readValue(String json, Class<T> valueType) throws JsonProcessingException, JsonMappingException {
        return MAPPER.readValue(json, valueType);
    }

    /**
     * Deserialize JSON content from given file into given Java type.
     *
     * @param file the string content for building the JSON tree
     * @param valueType the result class type
     * @param <T> the specific type of the result
     * @return The deserialized JSON content converted to the result type
     * @throws IOException if a low-level I/O problem occurs
     * @throws JsonParseException if underlying input contains invalid content
     * @throws JsonMappingException if the input JSON structure does not match structure expected for result type
     */
    public static <T> T readValue(File file, Class<T> valueType) throws IOException, JsonParseException {
        return MAPPER.readValue(file, valueType);
    }

    /**
     * Convenience method for doing two-step conversion from given value, into instance of given value type.
     *
     * @param node the Jackson JsonNode to convert
     * @param <T> the specific type of the result
     * @return The generated JSON that converted to the result type
     * @throws IllegalArgumentException if conversion fails due to incompatible type
     */
    public static <T> T convertValue(JsonNode node) {
        return MAPPER.convertValue(node, new TypeReference<T>(){});
    }

    /**
     * Serializing an object to a JSON as a string.
     *
     * @param object the object to convert
     * @return The generated JSON as a string
     */
    public static String toJsonString(Object object) {
        try {
            return MAPPER.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            return MAPPER.valueToTree(object).toString();
        }
    }

    /**
     * Serializing an object to a JSON as a string in pretty format.
     *
     * @param object the object to convert
     * @return The generated JSON as a string in pretty format
     */
    public static String toJsonPretty(Object object) {
        try {
            return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(object);
        } catch (JsonProcessingException e) {
            return MAPPER.valueToTree(object).toPrettyString();
        }
    }

    /**
     * Query data from a Jackson JsonNode by Josson query language.
     *
     * @param node the Jackson JsonNode that retrieve data from
     * @param jossonPath the Josson query path
     * @return The resulting Jackson JsonNode
     * @throws IllegalArgumentException if the query path is invalid
     */
    public static JsonNode getNode(JsonNode node, String jossonPath) {
        if (node == null || StringUtils.isBlank(jossonPath)) {
            return node;
        }
        List<String> keys = decomposePaths(jossonPath);
        if (keys.isEmpty()) {
            return node;
        }
        try {
            node = toValueNode(keys.get(0));
            keys.remove(0);
        } catch (NumberFormatException e) {
            // continue
        }
        return getNodeByKeys(node, keys);
    }
}
