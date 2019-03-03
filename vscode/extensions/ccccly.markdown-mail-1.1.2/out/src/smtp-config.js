"use strict";
const config = {
    "outlook": {
        "host": "smtp-mail.outlook.com",
        "tls": true
    },
    "qq": {
        "host": "smtp.qq.com",
        "ssl": "true"
    },
    "163": {
        "host": "smtp.163.com",
        "ssl": true
    },
    "126": {
        "host": "smtp.126.com",
        "ssl": true
    },
    "gmail": {
        "host": "smtp.gmail.com",
        "tls": true
    },
    "ecnu": {
        "host": "smail.ecnu.edu.cn",
        "port": 25
    }
};
function getSmtpConfig(emailAddress) {
    const domain = emailAddress.split('@')[1].split('.')[0];
    return config[`${domain}`];
}
Object.defineProperty(exports, "__esModule", { value: true });
exports.default = getSmtpConfig;
//# sourceMappingURL=smtp-config.js.map