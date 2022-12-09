/*
 * Copyright 2020-2022 Octomix Software Technology Limited
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
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;

import java.io.File;
import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.Objects;

import static com.octomix.josson.JossonCore.getPathByExpression;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.Utils.*;

/**
 * Josson is a query language for JSON.
 */
public class Josson {

    private JsonNode jsonNode;

    private Josson(final JsonNode node) {
        this.jsonNode = node;
    }

    /**
     * Create a Josson object that contains an empty Jackson {@code ObjectNode}.
     *
     * @return The new Josson object
     */
    public static Josson create() {
        return new Josson(createObjectNode());
    }

    /**
     * Create a Josson object with given Jackson JsonNode.
     *
     * @param node the Jackson JsonNode to store
     * @return The new Josson object
     * @throws NullPointerException if {@code node} is null
     */
    public static Josson create(final JsonNode node) {
        Objects.requireNonNull(node);
        return new Josson(node);
    }

    /**
     * Create a Josson object that contains an empty Jackson ArrayNode.
     *
     * @return The new Josson object
     */
    public static Josson createArray() {
        return new Josson(MAPPER.createArrayNode());
    }

    /**
     * Create a Josson object with given object that converted to an equivalent JSON Tree representation.
     *
     * @param object the object to convert
     * @return The new Josson object
     * @throws NullPointerException if {@code object} is null
     */
    public static Josson from(final Object object) {
        Objects.requireNonNull(object, "object must not be null");
        return new Josson(MAPPER.valueToTree(object));
    }

    /**
     * Create a Josson object with given JSON content string that deserialized to a Jackson JsonNode.
     *
     * @param json the string content for building the JSON tree
     * @return The new Josson object
     * @throws JsonProcessingException if the underlying input contains invalid content,
     *         IllegalArgumentException if {@code json} is null
     */
    public static Josson fromJsonString(final String json) throws JsonProcessingException {
        return new Josson(MAPPER.readTree(json));
    }

    /**
     * Set the Josson content with given JSON content string that deserialized to a Jackson JsonNode.
     *
     * @param json the string content for building the JSON tree
     * @throws JsonProcessingException if the underlying input contains invalid content,
     *         IllegalArgumentException if {@code json} is null
     */
    public void setJsonString(final String json) throws JsonProcessingException {
        jsonNode = MAPPER.readTree(json);
    }

    /**
     * Set the Josson content with given Jackson JsonNode.
     *
     * @param node the Jackson JsonNode to store
     * @throws NullPointerException if {@code node} is null
     */
    public void setNode(final JsonNode node) {
        Objects.requireNonNull(node);
        this.jsonNode = node;
    }

    /**
     * Add an entry of {@code Enum} name value to the current {@code ObjectNode}.
     *
     * @param key key of the entry
     * @param value value of the entry
     * @param <E> type of the {@code Enum}
     * @return {@code this}
     */
    public <E extends Enum<E>> Josson put(final String key, final Enum<E> value) {
        ((ObjectNode) jsonNode).put(key, value == null ? null : value.name());
        return this;
    }

    /**
     * Add an entry of {@code String} value to the current {@code ObjectNode}.
     *
     * @param key key of the entry
     * @param value value of the entry
     * @return {@code this}
     */
    public Josson put(final String key, final String value) {
        ((ObjectNode) jsonNode).put(key, value);
        return this;
    }

    /**
     * Add an entry of {@code Long} value to the current {@code ObjectNode}.
     *
     * @param key key of the entry
     * @param value value of the entry
     * @return {@code this}
     */
    public Josson put(final String key, final Long value) {
        ((ObjectNode) jsonNode).put(key, value);
        return this;
    }

    /**
     * Add an entry of {@code Integer} value to the current {@code ObjectNode}.
     *
     * @param key key of the entry
     * @param value value of the entry
     * @return {@code this}
     */
    public Josson put(final String key, final Integer value) {
        ((ObjectNode) jsonNode).put(key, value);
        return this;
    }

    /**
     * Add an entry of {@code Double} value to the current {@code ObjectNode}.
     *
     * @param key key of the entry
     * @param value value of the entry
     * @return {@code this}
     */
    public Josson put(final String key, final Double value) {
        ((ObjectNode) jsonNode).put(key, value);
        return this;
    }

    /**
     * Add an entry of {@code Boolean} value to the current {@code ObjectNode}.
     *
     * @param key key of the entry
     * @param value value of the entry
     * @return {@code this}
     */
    public Josson put(final String key, final Boolean value) {
        ((ObjectNode) jsonNode).put(key, value);
        return this;
    }

    /**
     * Add an entry of {@code LocalDateTime} value to the current {@code ObjectNode}.
     *
     * @param key key of the entry
     * @param value value of the entry
     * @return {@code this}
     */
    public Josson put(final String key, final LocalDateTime value) {
        ((ObjectNode) jsonNode).put(key, value.toString());
        return this;
    }

    /**
     * Add an entry of {@code JsonNode} value to the current {@code ObjectNode}.
     *
     * @param key key of the entry
     * @param value value of the entry
     * @return {@code this}
     */
    public Josson put(final String key, final JsonNode value) {
        ((ObjectNode) jsonNode).putPOJO(key, value);
        return this;
    }

    /**
     * Simply returns the content.
     *
     * @return The root Jackson JsonNode
     */
    public JsonNode getRoot() {
        return jsonNode;
    }

    /**
     * Simply returns the content if it is an {@code ObjectNode}.
     *
     * @return The root as Jackson {@code ObjectNode}. {@code null} if the root is not an {@code ObjectNode}.
     */
    public ObjectNode getRootAsObject() {
        return jsonNode != null && jsonNode.isObject() ? (ObjectNode) jsonNode : null;
    }

    /**
     * Simply returns the content if it is an {@code ArrayNode}.
     *
     * @return The root as Jackson {@code ArrayNode}. {@code null} if the root is not an {@code ArrayNode}.
     */
    public ArrayNode getRootAsArray() {
        return jsonNode != null && jsonNode.isArray() ? (ArrayNode) jsonNode : null;
    }

    /**
     * Simply returns the content.
     *
     * @return The root Jackson JsonNode
     */
    public JsonNode getNode() {
        return jsonNode;
    }

    /**
     * Get an {@code ArrayNode} element if the content is an {@code ArrayNode}.
     *
     * @param index index of the specific {@code ArrayNode} element
     * @return The specific {@code ArrayNode} element. {@code null} if the root is not an {@code ArrayNode}.
     */
    public JsonNode getNode(final int index) {
        return jsonNode != null && jsonNode.isArray() ? jsonNode.get(index) : null;
    }

    /**
     * Query data by Josson query language.
     *
     * @param expression the Josson query path
     * @return The resulting Jackson JsonNode
     * @throws IllegalArgumentException if the query path is invalid
     */
    public JsonNode getNode(final String expression) {
        return JossonCore.getNodeByExpression(jsonNode, expression);
    }

    /**
     * Query data on an element of {@code ArrayNode} by Josson query language.
     *
     * @param index index of the specific {@code ArrayNode} element
     * @param expression the Josson query path
     * @return The resulting Jackson JsonNode. {@code null} if the root is not an {@code ArrayNode}.
     * @throws IllegalArgumentException if the query path is invalid
     */
    public JsonNode getNode(final int index, final String expression) {
        return JossonCore.getNodeByExpression(jsonNode, index, expression);
    }

    /**
     * Query data from a Jackson JsonNode by Josson query language.
     *
     * @param node the Jackson JsonNode that retrieve data from
     * @param expression the Josson query path
     * @return The resulting Jackson JsonNode
     * @throws IllegalArgumentException if the query path is invalid
     */
    public static JsonNode getNode(final JsonNode node, final String expression) {
        return JossonCore.getNodeByExpression(node, expression);
    }

    /**
     * Query data on an element of ArrayNode by Josson query language.
     *
     * @param node the Jackson JsonNode that retrieve data from
     * @param index index of the specific ArrayNode element
     * @param expression the Josson query path
     * @return The resulting Jackson JsonNode. {@code null} if the root is not an ArrayNode.
     * @throws IllegalArgumentException if the query path is invalid
     */
    public static JsonNode getNode(final JsonNode node, final int index, final String expression) {
        return JossonCore.getNodeByExpression(node, index, expression);
    }

    /**
     * Query data by Josson query language.
     *
     * @param expression the Josson query path
     * @return A new Josson object with the resulting JsonNode
     */
    public Josson getJosson(final String expression) {
        final JsonNode node = getNode(expression);
        return node == null ? null : new Josson(node);
    }

    /**
     * Query data on an element of {@code ArrayNode} by Josson query language.
     *
     * @param index index of the specific {@code ArrayNode} element
     * @param expression the Josson query path
     * @return A new Josson object with the resulting JsonNode. {@code null} if the root is not an {@code ArrayNode}.
     */
    public Josson getJosson(final int index, final String expression) {
        final JsonNode node = getNode(index, expression);
        return node == null ? null : new Josson(node);
    }

    /**
     * Query data by Josson query language and return the path trace along the main branch.
     *
     * @param expression the Josson query path
     * @return A {@code PathTrace} object that contains all progressive nodes and variables defined along the main branch.
     * @throws IllegalArgumentException if the query path is invalid
     */
    public PathTrace getPathTrace(final String expression) {
        return getPathByExpression(PathTrace.from(jsonNode), expression);
    }

    /**
     * Query data by Josson query language, return result for {@code ArrayNode} only.
     *
     * @param expression the Josson query path
     * @return The resulting Jackson {@code ArrayNode}. {@code null} if the result is not an {@code ArrayNode}.
     * @throws IllegalArgumentException if the query path is invalid
     */
    public ArrayNode getArrayNode(final String expression) {
        final JsonNode node = getNode(expression);
        return node != null && node.isArray() ? (ArrayNode) node : null;
    }

    /**
     * Query data by Josson query language, return result for {@code ValueNode} only.
     *
     * @param expression the Josson query path
     * @return The resulting Jackson {@code ValueNode}. {@code null} if the result is not a {@code ValueNode}.
     * @throws IllegalArgumentException if the query path is invalid
     */
    public ValueNode getValueNode(final String expression) {
        final JsonNode node = getNode(expression);
        return node != null && node.isValueNode() ? (ValueNode) node : null;
    }

    /**
     * Query data by Josson query language, return result for {@code ValueNode} only.
     *
     * @param expression the Josson query path
     * @return The resulting Jackson {@code ValueNode}.
     * @throws IllegalArgumentException if the query path is invalid
     * @throws NoSuchElementException if the result is not a {@code ValueNode}
     */
    public ValueNode getRequiredValueNode(final String expression) {
        final JsonNode node = getNode(expression);
        if (node == null || !node.isValueNode()) {
            throw new NoSuchElementException("This Josson path cannot evaluate to a value node: " + expression);
        }
        return (ValueNode) node;
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code String}.
     *
     * @param expression the Josson query path
     * @return {@code node.asText()} if the result is a {@code ValueNode},
     *         {@code null} if no result found or the result is a {@code NullNode}, otherwise {@code node.toString()}
     * @throws IllegalArgumentException if the query path is invalid
     */
    public String getString(final String expression) {
        final JsonNode node = getNode(expression);
        return nodeIsNull(node) ? null : node.isValueNode() ? node.asText() : node.toString();
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code String}.
     *
     * @param expression the Josson query path
     * @return {@code node.asText()}
     * @throws IllegalArgumentException if the query path is invalid
     * @throws NoSuchElementException if no result found or the result is not a {@code ValueNode}
     */
    public String getRequiredString(final String expression) {
        return getRequiredValueNode(expression).asText();
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code Long}.
     *
     * @param expression the Josson query path
     * @return {@code node.asLong()} if the result is a {@code ValueNode}, otherwise {@code null}
     * @throws IllegalArgumentException if the query path is invalid
     */
    public Long getLong(final String expression) {
        final ValueNode node = getValueNode(expression);
        return nodeIsNull(node) ? null : node.asLong();
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code Long}.
     *
     * @param expression the Josson query path
     * @return {@code node.asLong()}
     * @throws IllegalArgumentException if the query path is invalid
     * @throws NoSuchElementException if no result found or the result is not a {@code ValueNode}
     */
    public Long getRequiredLong(final String expression) {
        return getRequiredValueNode(expression).asLong();
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code Integer}.
     *
     * @param expression the Josson query path
     * @return {@code node.asInt()} if the result is a {@code ValueNode}, otherwise {@code null}
     * @throws IllegalArgumentException if the query path is invalid
     */
    public Integer getInteger(final String expression) {
        final ValueNode node = getValueNode(expression);
        return nodeIsNull(node) ? null : node.asInt();
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code Integer}.
     *
     * @param expression the Josson query path
     * @return {@code node.asInt()}
     * @throws IllegalArgumentException if the query path is invalid
     * @throws NoSuchElementException if no result found or the result is not a {@code ValueNode}
     */
    public Integer getRequiredInteger(final String expression) {
        return getRequiredValueNode(expression).asInt();
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code Double}.
     *
     * @param expression the Josson query path
     * @return {@code node.asDouble()} if the result is a {@code ValueNode}, otherwise {@code null}
     * @throws IllegalArgumentException if the query path is invalid
     */
    public Double getDouble(final String expression) {
        final ValueNode node = getValueNode(expression);
        return nodeIsNull(node) ? null : node.asDouble();
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code Double}.
     *
     * @param expression the Josson query path
     * @return {@code node.asDouble()}
     * @throws IllegalArgumentException if the query path is invalid
     * @throws NoSuchElementException if no result found or the result is not a {@code ValueNode}
     */
    public Double getRequiredDouble(final String expression) {
        return getRequiredValueNode(expression).asDouble();
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code Boolean}.
     *
     * @param expression the Josson query path
     * @return {@code node.asBoolean()} if the result is a {@code ValueNode}, otherwise {@code null}
     * @throws IllegalArgumentException if the query path is invalid
     */
    public Boolean getBoolean(final String expression) {
        final ValueNode node = getValueNode(expression);
        return nodeIsNull(node) ? null : node.asBoolean();
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code Boolean}.
     *
     * @param expression the Josson query path
     * @return {@code node.asBoolean()}
     * @throws IllegalArgumentException if the query path is invalid
     * @throws NoSuchElementException if no result found or the result is not a {@code ValueNode}
     */
    public Boolean getRequiredBoolean(final String expression) {
        return getRequiredValueNode(expression).asBoolean();
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code LocalDateTime}.
     *
     * @param expression the Josson query path
     * @return parsed {@code LocalDateTime} if the result is a {@code TextNode}, otherwise {@code null}
     * @throws IllegalArgumentException if the query path is invalid or the text cannot be parsed
     */
    public LocalDateTime getIsoLocalDateTime(final String expression) {
        final ValueNode node = getValueNode(expression);
        return node == null || !node.isTextual() ? null : toLocalDateTime(node);
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code LocalDateTime}.
     *
     * @param expression the Josson query path
     * @return parsed {@code LocalDateTime} if the result is a {@code TextNode}, otherwise {@code null}
     * @throws IllegalArgumentException if the query path is invalid or the text cannot be parsed
     * @throws NoSuchElementException if no result found or the result is not a {@code ValueNode}
     */
    public LocalDateTime getRequiredIsoLocalDateTime(final String expression) {
        final ValueNode node = getRequiredValueNode(expression);
        return !node.isTextual() ? null : toLocalDateTime(node);
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code LocalDate}.
     *
     * @param expression the Josson query path
     * @return parsed {@code LocalDate} if the result is a {@code TextNode}, otherwise {@code null}
     * @throws IllegalArgumentException if the query path is invalid or the text cannot be parsed
     */
    public LocalDate getIsoLocalDate(final String expression) {
        final ValueNode node = getValueNode(expression);
        return node == null || !node.isTextual() ? null : LocalDate.parse(node.asText());
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code LocalDate}.
     *
     * @param expression the Josson query path
     * @return parsed {@code LocalDate} if the result is a {@code TextNode}, otherwise {@code null}
     * @throws IllegalArgumentException if the query path is invalid or the text cannot be parsed
     * @throws NoSuchElementException if no result found or the result is not a {@code ValueNode}
     */
    public LocalDate getRequiredIsoLocalDate(final String expression) {
        final ValueNode node = getRequiredValueNode(expression);
        return !node.isTextual() ? null : LocalDate.parse(node.asText());
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code OffsetDateTime}.
     *
     * @param expression the Josson query path
     * @return parsed {@code OffsetDateTime} if the result is a {@code TextNode}, otherwise {@code null}
     * @throws IllegalArgumentException if the query path is invalid or the text cannot be parsed
     */
    public OffsetDateTime getOffsetDateTime(final String expression) {
        final ValueNode node = getValueNode(expression);
        return node == null || !node.isTextual() ? null : toOffsetDateTime(node);
    }

    /**
     * Query a {@code ValueNode} by Josson query language, return the result as {@code OffsetDateTime}.
     *
     * @param expression the Josson query path
     * @return parsed {@code OffsetDateTime} if the result is a {@code TextNode}, otherwise {@code null}
     * @throws IllegalArgumentException if the query path is invalid or the text cannot be parsed
     * @throws NoSuchElementException if no result found or the result is not a {@code ValueNode}
     */
    public OffsetDateTime getRequiredOffsetDateTime(final String expression) {
        final ValueNode node = getRequiredValueNode(expression);
        return !node.isTextual() ? null : toOffsetDateTime(node);
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
     * Convert the content into instance of given value type.
     *
     * @param type the specific type of the result
     * @return The generated JSON that converted to the result type
     * @param <T> the specific type of the result
     * @throws IOException if conversion fails due to incompatible type
     */
    public <T> T readValueFor(final Class<?> type) throws IOException {
        return MAPPER.readerFor(type).readValue(jsonNode);
    }

    /**
     * Convenience method for doing two-step conversion from given value, into instance of given value type.
     *
     * @param node the Jackson JsonNode to convert
     * @param type the specific type of the result
     * @return The generated JSON that converted to the result type
     * @param <T> the specific type of the result
     * @throws IOException if conversion fails due to incompatible type
     */
    public static <T> T readValueFor(final JsonNode node, final Class<?> type) throws IOException {
        return MAPPER.readerFor(type).readValue(node);
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
     * Convenience method for doing two-step conversion from given value, into instance of given value type.
     *
     * @param node the Jackson JsonNode to convert
     * @param <T> the specific type of the result
     * @return The generated JSON that converted to the result type
     * @throws IllegalArgumentException if conversion fails due to incompatible type
     */
    public static <T> T convertValue(final JsonNode node) {
        return MAPPER.convertValue(node, new TypeReference<T>(){});
    }

    /**
     * Set serializing inclusion options.
     *
     * @param include JsonInclude.Include e.g. NON_NULL
     */
    public static void setSerializationInclusion(final JsonInclude.Include include) {
        MAPPER.setSerializationInclusion(include);
    }

    /**
     * To tailor the output of the function formatDate().
     * Default value is Locale.getDefault().
     *
     * @param locale a geographical, political, or cultural region.
     */
    public static void setLocale(final Locale locale) {
        JossonCore.setLocale(locale);
    }

    /**
     * Get the current locale setting.
     * Default value is Locale.getDefault().
     *
     * @return The current {@code Local} setting.
     */
    public static Locale getLocale() {
        return JossonCore.getLocale();
    }

    /**
     * To identify the rules used to convert between an OffsetDateTime and a LocalDateTime.
     * Default value is ZoneId.systemDefault().
     *
     * @param zoneId a time-zone ID
     */
    public static void setZoneId(final ZoneId zoneId) {
        JossonCore.setZoneId(zoneId);
    }

    /**
     * Get the current time-zone setting.
     * Default value is ZoneId.systemDefault().
     *
     * @return The current {@code ZoneId} setting
     */
    public static ZoneId getZoneId() {
        return JossonCore.getZoneId();
    }

    /**
     * Create an empty Jackson {@code ObjectNode}.
     *
     * @return The new empty Jackson {@code ObjectNode}
     */
    public static ObjectNode createObjectNode() {
        return MAPPER.createObjectNode();
    }

    /**
     * Create an empty Jackson {@code ArrayNode}.
     *
     * @return The new empty Jackson {@code ArrayNode}
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
    public static JsonNode readJsonNode(final Object object) {
        return MAPPER.valueToTree(object);
    }

    /**
     * Deserialize JSON content string to a Jackson JsonNode.
     *
     * @param json the string content for building the JSON tree
     * @return A JsonNode, if valid JSON content found
     * @throws JsonProcessingException if underlying input contains invalid content
     */
    public static JsonNode readJsonNode(final String json) throws JsonProcessingException {
        if (json == null) {
            return null;
        }
        return MAPPER.readTree(json);
    }

    /**
     * Convert an object to a Jackson {@code ObjectNode}.
     *
     * @param object the object to convert
     * @return The resulting Jackson {@code ObjectNode}
     * @throws IllegalArgumentException if the converted JSON is not a Jackson object
     */
    public static ObjectNode readObjectNode(final Object object) {
        final JsonNode node = MAPPER.valueToTree(object);
        if (node.isObject()) {
            return (ObjectNode) node;
        }
        throw new IllegalArgumentException("The provided object is not an object node");
    }

    /**
     * Convert an object to a Jackson {@code ArrayNode}.
     *
     * @param object the object to convert
     * @return The resulting Jackson {@code ArrayNode}
     * @throws IllegalArgumentException if the converted JSON is not a Jackson array
     */
    public static ArrayNode readArrayNode(final Object object) {
        final JsonNode node = MAPPER.valueToTree(object);
        if (node.isArray()) {
            return (ArrayNode) node;
        }
        throw new IllegalArgumentException("The provided object is not an array node");
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
    public static <T> T readValue(final String json,
                                  final Class<T> valueType) throws JsonProcessingException, JsonMappingException {
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
     */
    public static <T> T readValue(final File file, final Class<T> valueType) throws IOException {
        return MAPPER.readValue(file, valueType);
    }

    /**
     * Serializing an object to a JSON as a string.
     *
     * @param object the object to convert
     * @return The generated JSON as a string
     */
    public static String toJsonString(final Object object) {
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
    public static String toJsonPretty(final Object object) {
        try {
            return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(object);
        } catch (JsonProcessingException e) {
            return MAPPER.valueToTree(object).toPrettyString();
        }
    }
}
