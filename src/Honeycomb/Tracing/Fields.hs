{-# LANGUAGE OverloadedStrings #-}
module Honeycomb.Tracing.Fields where
import Data.Text (Text)

-- ** user namespace
userFieldNamespace :: Text -> Text
userFieldNamespace = ("app." <>)

-- ** trace namespace
{- | Unique ID (within trace) for a given event/span. -}
spanIdField :: Text
spanIdField = "trace.span_id"

{- | ID of a span's parent. No value if at the root. -}
parentIdField :: Text
parentIdField = "trace.parent_id"

{- | Global ID -}
traceIdField :: Text
traceIdField = "trace.trace_id"

-- ** 'global' namespace
{-
  | The type of a Span usually represents what aspect of an application the Span covers
    (e.g. http_server, http_client).
  -}
typeField :: Text
typeField = "type"

{- | Name of the application being instrumented. -}
serviceNameField :: Text
serviceNameField = "service_name"

{- | Name of the operation the span covers. -}
spanNameField :: Text
spanNameField = "name"

{- | Duration the operation took, in milliseconds (as a double). -}
durationField :: Text
durationField = "duration_ms"

-- ** meta namespace
{- | Name of the package under instrumentation. -}
packageField :: Text
packageField = "meta.package"

{- Version string of the package under instrumentation. -}
packageVersionField :: Text
packageVersionField = "meta.package_version"

{- Version of the Beeline instrumentation. -}
beelineVersionField :: Text
beelineVersionField = "meta.beeline_version"

{- List of names showing what instrumentations are active. -}
instrumentationsField :: Text
instrumentationsField = "meta.instrumentations"

{- length of meta.instrumentations list. -}
instrumentationsCountField :: Text
instrumentationsCountField = "meta.instrumentation_count"

{- os.hostname() aka server name -}
localHostnameField :: Text
localHostnameField = "meta.local_hostname"

{- Field key for specifying a span type as a Span Event -}
metaAnnotationTypeField :: Text
metaAnnotationTypeField = "meta.annotation_type"

{- Field value for specifying a span type as a Span Event -}
metaTypeSpanEventValue :: Text
metaTypeSpanEventValue = "span_event"

{- Field value for specifying a span type as a Span Event -}
metaTypeLinkEventValue :: Text
metaTypeLinkEventValue = "link"

-- ** request namespace
{- | Value of the HTTP Host header. -}
requestHostField :: Text
requestHostField = "request.host"

{- | HTTP method value. -}
requestMethodField :: Text
requestMethodField = "request.method"

{- | HTTP protocol version. -}
requestHttpVersionField :: Text
requestHttpVersionField = "request.http_version"

{- | The path component of the request URL. -}
requestPathField :: Text
requestPathField = "request.path"

{- | The scheme of the request URL (http/https). -}
requestSchemeField :: Text
requestSchemeField = "request.scheme"

{- | True if the connection is TLS/SSL. -}
requestSecureField :: Text
requestSecureField = "request.secure"

{- | Content length of the request - if known. -}
requestContentLengthField :: Text
requestContentLengthField = "request.content_length"

{- | The client IP address. -}
requestRemoteAddressField :: Text
requestRemoteAddressField = "request.remote_addr"

{- | True if the request is an AJAX request. -}
requestAjaxField :: Text
requestAjaxField = "request.xhr"

{- | The value of the 'user agent' header. -}
userAgentField :: Text
userAgentField = "request.header.user_agent"

{- | The value of the 'forwarded for' header. -}
forwardForHeaderField :: Text
forwardForHeaderField = "request.header.x_forwarded_for"

{- | The value of the 'forwarded proto' header -}
forwardProtoHeaderField :: Text
forwardProtoHeaderField = "request.header.x_forwarded_proto"

{- | Name of an error encountered that caused the request to fail (e.g. the name of an exception). -}
requestErrorField :: Text
requestErrorField = "request.error"

{- | Detail about the error (e.g. the exception's message). -}
requestErrorDetailField :: Text
requestErrorDetailField = "request.error_detail"

{- | The request content type - if available. -}
requestContentTypeField :: Text
requestContentTypeField = "request.header.content_type"

{- | The request accept header - if available. -}
requestAcceptField :: Text
requestAcceptField = "request.header.accept"

{- | Any query parameters on the request URL. -}
requestQueryParamsField :: Text
requestQueryParamsField = "request.query"

-- ========= http client namespace =========
{- -}
clientRequestPathField :: Text
clientRequestPathField = "client.request.path"

{- -}
clientRequestMethodField :: Text
clientRequestMethodField = "client.request.method"

{- -}
clientRequestContentLengthField :: Text
clientRequestContentLengthField = "client.request.content_length"

{- -}
clientRequestContentTypeField :: Text
clientRequestContentTypeField = "client.request.content_type"

{- -}
clientRequestErrorField :: Text
clientRequestErrorField = "client.request.error"

{- -}
clientRequestErrorDetailField :: Text
clientRequestErrorDetailField = "client.request.error_detail"

{- -}
clientResponseContentTypeField :: Text
clientResponseContentTypeField = "client.response.content_type"

{- -}
clientResponseStatusCodeField :: Text
clientResponseStatusCodeField = "client.response.status_code"

{- -}
clientResponseContentLength :: Text
clientResponseContentLength = "client.response.content_length"

-- ========= response namespace =========
{- | The response status code. -}
statusCodeField :: Text
statusCodeField = "response.status_code"

{- | The response content type - if available. -}
responseContentTypeField :: Text
responseContentTypeField = "response.header.content_type"

-- ========= database namespace =========
{- | The database query being run -}
databaseQueryField :: Text
databaseQueryField = "db.query"

{- | The database query parameters -}
databaseQueryParametersField :: Text
databaseQueryParametersField = "db.query_params"

databaseConnectionIdField :: Text
databaseConnectionIdField = "db.connection_id"

{- | The size of database batch being executed -}
databaseBatchSizeField :: Text
databaseBatchSizeField = "db.batch_size"

{- | Is the query part of a batch execution -}
databaseIsBatchField :: Text
databaseIsBatchField = "db.is_batch"

databaseStatementTypeField :: Text
databaseStatementTypeField = "db.statement_type"

{- | Whether database query was successful -}
databaseIsSuccess :: Text
databaseIsSuccess = "db.is_success"

{- | Name of an error encountered that caused the request to fail (e.g. the name of an exception). -}
databaseError :: Text
databaseError = "db.error"

{- | Detail about the error (e.g. the exception's message). -}
databaseErrorDetails :: Text
databaseErrorDetails = "db.error_details"

-- ** indicators of problems
{- | A child span sent as result of its parent being closed before it was closed. -}
metaSentByParentField :: Text
metaSentByParentField = "meta.sent_by_parent"

{- | A span sent during cleanup, indicating a public API misuse (e.g. a trace was not properly closed). -}
metaDirtyContextField :: Text
metaDirtyContextField = "meta.dirty_context"