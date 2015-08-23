/**
 * Created by liutos on 15-8-22.
 */

$(document).on('click', '.request-headers-field-add', function (event) {
    var btn = event.currentTarget;
    console.log(btn);
    var row = $(btn.closest('.request-headers-field'));
    console.log(row);
    var html = new EJS({ url: '/static/tmpl/request-headers-field-tmpl.html' }).render({});
    console.log(html);
    row.next().after(html);
});

$(document).on('click', '.request-headers-field-delete', function (event) {
    var btn = event.currentTarget;
    var row = $(btn.closest('.request-headers-field'));
    console.log(row);
    var next = row.next().next();
    console.log(next.length);
    var prev = row.prev();
    console.log(prev.length);
    if (next.length > 0 || prev.length > 0) {
        row.next().remove();
        row.remove();
    } else {
        var input = row.find('input');
        input.val('');
    }
});

$(document).on('click', '.request-param-add', function (event) {
    var btn = event.currentTarget;
    var row = $(btn.closest('.request-param'));
    console.log(row);
    var html = new EJS({ url: '/static/tmpl/request-param-tmpl.html' }).render({});
    console.log(html);
    row.next().after(html);
});

$(document).on('click', '.request-param-delete', function (event) {
    var btn = event.currentTarget;
    var row = $(btn.closest('.request-param'));
    console.log(row);
    var next = row.next().next();
    console.log(next.length);
    var prev = row.prev();
    console.log(prev.length);
    if (next.length > 0 || prev.length > 0) {
        row.next().remove();
        row.remove();
    } else {
        var input = row.find('input');
        input.val('');
    }
});

function collect_headers() {
    var headers = {};
    $('.request-headers-field').each(function (_, header) {
        var key = $(header).find('.input-header-key');
        var val = $(header).find('.input-header-value');
        console.log(key);
        console.log(val);
        if (key.val() === "") {
            return;
        }
        headers[key.val()] = val.val();
    });
    return JSON.stringify(headers);
}

function collect_params() {
    var params = {};
    $('.request-param').each(function (_, param) {
        var key = $(param).find('.input-param-key');
        var val = $(param).find('.input-param-value');
        console.log(key);
        console.log(val);
        if (key.val() === "") {
            return;
        }
        params[key.val()] = val.val();
    });
    return JSON.stringify(params);
}

function fill_resp_headers(headers) {
    var html = new EJS({ url: '/static/tmpl/response-headers-tmpl.html' }).render({ headers: headers });
    $('#response-headers').html(html);
}

$('#request-send').on('click', function () {
    var headers = collect_headers();
    var params = collect_params();
    var method = $('#input-method').val();
    var url = $('#input-url').val();
    var api = '/api/request';
    $.ajax(api, {
        data: {
            headers: headers,
            method: method,
            params: params,
            url: url
        },
        error: function () {
            alert('Error occurred at '.concat(api));
        },
        success: function (data) {
            $('#response-info').show();
            console.log(data);
            var headers = data['DATA']['HEADERS'];
            fill_resp_headers(headers);
            var body = data['DATA']['BODY'];
            $('#response-body-raw-content').text(body);
        }
    });
});

(function () {
    var html = new EJS({ url: '/static/tmpl/request-headers-field-tmpl.html' }).render({});
    $('#request-headers').html(html);
})();

(function () {
    var html = new EJS({ url: '/static/tmpl/request-param-tmpl.html' }).render({});
    $('#request-params').html(html);
})();