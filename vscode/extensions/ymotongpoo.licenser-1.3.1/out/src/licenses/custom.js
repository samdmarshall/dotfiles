//    Copyright 2016 David Lechner, Yoshi Yamaguchi
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//        http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
class Custom {
    constructor(author, project, customTermsAndConditions, customHeader, filePath) {
        this.author = author;
        let date = new Date();
        this.year = date.getFullYear().toString();
        this.project = project;
        this.customTermsAndConditions = customTermsAndConditions;
        this.customHeader = customHeader;
        this.filePath = path.parse(filePath);
    }
    replaceVariables(text) {
        text = text.replace(/@AUTHOR@/g, this.author);
        text = text.replace(/@YEAR@/g, this.year);
        text = text.replace(/@PROJECT@/g, this.project);
        text = text.replace(/@FILE@/g, this.filePath.base);
        return text;
    }
    termsAndConditions() {
        let template = this.replaceVariables(this.customTermsAndConditions);
        return template;
    }
    header() {
        let template = this.replaceVariables(this.customHeader);
        return template;
    }
}
exports.Custom = Custom;
//# sourceMappingURL=custom.js.map