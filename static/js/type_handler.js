/**
 * Created by liutos on 15-8-25.
 */

$(document).on('click', '.type-raw', function (event) {
    var a = event.currentTarget;
    var row = $(a.closest('.kv-row'));
    var btn = row.find('.type-selected');
    btn.text($(a).text());
    var ins = row.children('.input-instance');
    var html = new EJS({ url: '/static/tmpl/type_handlers/th_raw-tmpl.html' }).render({});
    ins.html(html);
});

$(document).on('click', '.type-integer', function (event) {
    var a = event.currentTarget;
    var row = $(a.closest('.kv-row'));
    var btn = row.find('.type-selected');
    btn.text($(a).text());
    var ins = row.children('.input-instance');
    var html = new EJS({ url: '/static/tmpl/type_handlers/th_integer-tmpl.html' }).render({});
    ins.html(html);
});

$(document).on('click', '.type-timestamp', function (event) {
    var a = event.currentTarget;
    var row = $(a.closest('.kv-row'));
    var btn = row.find('.type-selected');
    btn.text($(a).text());
    var ins = row.children('.input-instance');
    var html = new EJS({ url: '/static/tmpl/type_handlers/th_timestamp-tmpl.html' }).render({});
    ins.html(html);
});

$(document).on('click', '.type-sign', function (event) {
    var a = event.currentTarget;
    var row = $(a.closest('.kv-row'));
    var btn = row.find('.type-selected');
    btn.text($(a).text());
    var ins = row.children('.input-instance');
    var html = new EJS({ url: '/static/tmpl/type_handlers/th_sign-tmpl.html' }).render({});
    ins.html(html);
});