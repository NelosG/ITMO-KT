package ru.ifmo.pga.software.design.news.module.stats.parser.impl

import com.google.gson.JsonObject
import com.google.gson.JsonParser
import org.apache.commons.lang3.Validate
import ru.ifmo.pga.software.design.news.module.stats.parser.VkResponseParser
import ru.ifmo.pga.software.design.news.module.stats.parser.exception.VkResponseParserException
import ru.ifmo.pga.software.design.news.module.stats.response.impl.VkNewsfeedSearchResponse
import java.util.*

class VkNewsfeedSearchResponseParser : VkResponseParser<VkNewsfeedSearchResponse> {

    override fun parse(apiResponse: String): VkNewsfeedSearchResponse {
        val rootJsonElement = JsonParser.parseString(apiResponse)
        val rootJsonObject = rootJsonElement.asJsonObject
        if (!rootJsonObject.has(RESPONSE_MEMBER_NAME)) {
            throwApiException(rootJsonObject)
        }
        val jsonObject = rootJsonObject.getAsJsonObject(RESPONSE_MEMBER_NAME)
        checkHasMember(jsonObject, COUNT_MEMBER_NAME)
        checkHasMember(jsonObject, TOTAL_COUNT_MEMBER_NAME)
        val count = jsonObject.getAsJsonPrimitive(COUNT_MEMBER_NAME).asInt
        val totalCount = jsonObject.getAsJsonPrimitive(TOTAL_COUNT_MEMBER_NAME).asLong
        if (!jsonObject.has(NEXT_FROM_MEMBER_NAME)) {
            return VkNewsfeedSearchResponse(count, Optional.empty(), totalCount)
        }
        val nextFrom = jsonObject.getAsJsonPrimitive(NEXT_FROM_MEMBER_NAME).asString
        return VkNewsfeedSearchResponse(count, Optional.of(nextFrom), totalCount)
    }

    private fun throwApiException(rootJsonObject: JsonObject) {
        checkHasMember(rootJsonObject, ERROR_MEMBER_NAME)
        val errorJsonObject = rootJsonObject.getAsJsonObject(ERROR_MEMBER_NAME)
        checkHasMember(errorJsonObject, ERROR_MESSAGE_MEMBER_NAME)
        throw VkResponseParserException(errorJsonObject.getAsJsonPrimitive(ERROR_MESSAGE_MEMBER_NAME).asString)
    }

    private fun checkHasMember(jsonObject: JsonObject, member: String) {
        Validate.isTrue(jsonObject.has(member), "Expected '$member' in json")
    }

    companion object {
        private const val RESPONSE_MEMBER_NAME = "response"
        private const val COUNT_MEMBER_NAME = "count"
        private const val NEXT_FROM_MEMBER_NAME = "next_from"
        private const val TOTAL_COUNT_MEMBER_NAME = "total_count"
        private const val ERROR_MEMBER_NAME = "error"
        private const val ERROR_MESSAGE_MEMBER_NAME = "error_msg"
    }
}