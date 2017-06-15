CREATE TABLE "COLLECTION" (
    "F_COLLECTION_ID" integer NOT NULL PRIMARY KEY,
    "F_COLLECTION_NAME" varchar(255) NOT NULL,
    "F_COLLECTION_YEAR" integer,
    "F_SERIA_ID" integer,
    "F_COLLECTION_VAL" integer,
    "F_PUBLISHING_ID" integer,
    "F_COLLECTION_PLACE" varchar(255),
    "F_COLLECTION_TESIS" bool,
    "F_COLLECTION_ABSTRACT" text NOT NULL,
    "F_COLLECTION_XML" text,
    "F_COLLECTION_USER" integer,
    UNIQUE ("F_COLLECTION_NAME", "F_COLLECTION_YEAR", "F_SERIA_ID", "F_COLLECTION_VAL", "F_COLLECTION_PLACE", "F_COLLECTION_TESIS")
);
CREATE TABLE "CORRECTOR" (
    "F_CORRECTOR_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL,
    "F_COLLECTION_ID" integer NOT NULL REFERENCES "COLLECTION" ("F_COLLECTION_ID"),
    "F_CORRECTOR_USER" integer
);
CREATE TABLE "SERIA" (
    "F_SERIA_ID" integer NOT NULL PRIMARY KEY,
    "F_SERIA_NAME" varchar(255) NOT NULL UNIQUE,
    "F_SERIA_ABSTRACT" text NOT NULL,
    "F_SERIA_XML" text,
    "F_SERIA_USER" integer
);
CREATE TABLE "JOURNAL" (
    "F_JOURNAL_ID" integer NOT NULL PRIMARY KEY,
    "F_JOURNAL_NAME" varchar(255) NOT NULL,
    "F_PUBLISHING_ID" integer,
    "F_JOURNAL_URL" varchar(255),
    "LANG_ID" integer,
    "F_JOURNAL_ABSTRACT" text NOT NULL,
    "F_JOURNAL_XML" text,
    "F_JOURNAL_USER" integer,
    UNIQUE ("F_JOURNAL_NAME", "F_PUBLISHING_ID")
);
CREATE TABLE "JALIAS" (
    "F_JALIAS_ID" integer NOT NULL PRIMARY KEY,
    "F_JALIAS_NAME" varchar(255) NOT NULL,
    "F_JOURNAL_ID" integer NOT NULL REFERENCES "JOURNAL" ("F_JOURNAL_ID"),
    "F_ALIAS_ID" integer,
    "F_JALIAS_USER" integer,
    UNIQUE ("F_JALIAS_NAME", "F_JOURNAL_ID", "F_ALIAS_ID")
);
CREATE TABLE "ALIAS" (
    "F_ALIAS_ID" integer NOT NULL PRIMARY KEY,
    "F_ALIAS_NAME" varchar(255) NOT NULL UNIQUE
);
CREATE TABLE "REDJOURNAL" (
    "F_REDJOURNAL_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL,
    "F_JOURNAL_ID" integer NOT NULL REFERENCES "JOURNAL" ("F_JOURNAL_ID"),
    "F_REDJOURNAL_BEGIN" date NOT NULL,
    "F_REDJOURNAL_END" date,
    "F_REDJOURNAL_USER" integer
);
CREATE TABLE "PATENT" (
    "F_PATENT_ID" integer NOT NULL PRIMARY KEY,
    "F_PATENT_NAME" varchar(255) NOT NULL,
    "F_PATENT_NUM" varchar(255) NOT NULL,
    "F_PATENT_DATE" date NOT NULL,
    "F_PATENT_FORMULA" text NOT NULL,
    "F_PATENT_XML" text NOT NULL,
    "F_PATENT_USER" integer
);
CREATE TABLE "AUTHORP" (
    "F_AUTHORP_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL,
    "F_PATENT_ID" integer NOT NULL REFERENCES "PATENT" ("F_PATENT_ID"),
    "F_AUTHORP_NAME" varchar(255) NOT NULL,
    "F_AUTHORP_ORD" integer,
    "F_AUTHORP_USER" integer
);
CREATE TABLE "ARTICLE" (
    "F_ARTICLE_ID" integer NOT NULL PRIMARY KEY,
    "F_ARTICLE_NAME" varchar(255) NOT NULL,
    "F_JOURNAL_ID" integer REFERENCES "JOURNAL" ("F_JOURNAL_ID"),
    "F_ARTICLE_VOL" integer,
    "F_ARTICLE_NUM" integer,
    "F_ARTICLE_YEAR" integer,
    "F_COLLECTION_ID" integer REFERENCES "COLLECTION" ("F_COLLECTION_ID"),
    "F_ARTICLE_FIRSTPAGE" integer,
    "F_ARTICLE_LASTPAGE" integer,
    "F_ARTICLE_ABSTRACT" text,
    "LANG_ID" integer,
    "F_ARTICLE_XML" text,
    "F_ARTICLE_USER" integer
);
CREATE TABLE "AUTHORA" (
    "F_AUTHORA_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL,
    "F_ARTICLE_ID" integer NOT NULL REFERENCES "ARTICLE" ("F_ARTICLE_ID"),
    "F_AUTHORA_NAME" varchar(200),
    "F_AUTHORA_ORD" integer,
    "F_AUTHORA_USER" integer
);
CREATE TABLE "BOOK" (
    "F_BOOK_ID" integer NOT NULL PRIMARY KEY,
    "F_BOOK_NAME" varchar(255) NOT NULL,
    "F_BOOK_YEAR" integer NOT NULL,
    "F_PUBLISHING_ID" integer,
    "F_BOOK_PLACE" varchar(255),
    "F_BOOK_PAGES" integer,
    "F_BOOK_ABSTRACT" text,
    "LANG_ID" integer,
    "F_BOOK_XML" text,
    "F_BOOK_USER" integer
);
CREATE TABLE "AUTHORB" (
    "F_AUTHORB_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL,
    "F_BOOK_ID" integer NOT NULL REFERENCES "BOOK" ("F_BOOK_ID"),
    "F_AUTHORB_NAME" varchar(200),
    "F_AUTHORB_ORD" integer,
    "F_AUTHORB_USER" integer
);
CREATE TABLE "T_LANGS" (
    "LANG_ID" integer NOT NULL PRIMARY KEY
);
CREATE TABLE "FILES" (
    "F_FILES_ID" integer NOT NULL PRIMARY KEY,
    "F_FILES_LINK" varchar(255) NOT NULL,
    "F_FILES_TYPE" varchar(255) NOT NULL,
    "F_FILES_DESCIPTION" varchar(1000) NOT NULL,
    "F_ARTICLE_ID" integer REFERENCES "ARTICLE" ("F_ARTICLE_ID"),
    "F_BOOK_ID" integer REFERENCES "BOOK" ("F_BOOK_ID"),
    "F_FILES_USER" integer
);
CREATE TABLE "LOGPARS" (
    "F_LOGPARS_ID" integer NOT NULL PRIMARY KEY,
    "F_LOGPARS_USER" integer,
    "F_LOGPARS_SOURCE" text,
    "F_LOGPARS_AUTODATA" text,
    "F_LOGPARS_USERDATA" text,
    "F_LOGPARS_TYPE" varchar(255),
    "F_LOGPARS_METHOD" varchar(255),
    "F_LOGPARS_CREATE" datetime
);
CREATE TABLE "PUBLISHING" (
    "F_PUBLISHING_ID" integer NOT NULL PRIMARY KEY,
    "F_PUBLISHING_NAME" varchar(255) NOT NULL,
    "F_PUBLISHING_TOWN" varchar(255) NOT NULL,
    UNIQUE ("F_PUBLISHING_NAME", "F_PUBLISHING_TOWN")
);
CREATE TABLE "workers_profile" (
    "id" integer NOT NULL PRIMARY KEY,
    "user_id" integer NOT NULL UNIQUE,
    "creation_date" datetime NOT NULL,
    "country" varchar(2),
    "latitude" decimal,
    "longitude" decimal,
    "location" varchar(255),
    "firstname" varchar(255) NOT NULL,
    "middlename" varchar(255) NOT NULL,
    "lastname" varchar(255) NOT NULL,
    "worker_id" integer UNIQUE
);
CREATE TABLE "MAN" (
    "F_MAN_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_NAMEL" varchar(255) NOT NULL,
    "F_MAN_NAMEF" varchar(255),
    "F_MAN_NAMEM" varchar(255),
    "F_MAN_BIRTH" date,
    "F_MAN_USER" integer
);
CREATE TABLE "MANALIAS" (
    "F_MANALIAS_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "F_MANALIAS_NAMEL" varchar(255) NOT NULL,
    "F_MANALIAS_NAMEF" varchar(255),
    "F_MANALIAS_NAMEM" varchar(255),
    "F_MANALIAS_ISCORRECT" bool NOT NULL,
    "F_MANALIAS_USER" integer
);
CREATE TABLE "MANSPOST" (
    "F_MANSPOST_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "F_DEPARTMENT_ID" integer NOT NULL,
    "F_MANSPOST_BEGIN" date NOT NULL,
    "F_MANSPOST_END" date,
    "F_MANSPOST_PART" bool,
    "F_MANSPOST_USER" integer,
    UNIQUE ("F_MAN_ID", "F_DEPARTMENT_ID", "F_MANSPOST_BEGIN")
);
CREATE TABLE "PATPROGRAM" (
    "F_PATPROGRAM_ID" integer NOT NULL PRIMARY KEY,
    "F_PATPROGRAM_NAME" varchar(255) NOT NULL,
    "F_PATPROGRAM_NUMBER" varchar(255) NOT NULL,
    "F_PATPROGRAM_DATE" date NOT NULL,
    "F_PATPROGRAM_DESCRIPTION" text NOT NULL,
    "F_PATPROGRAM_XML" text NOT NULL,
    "F_PATPROGRAM_USER" integer
);
CREATE TABLE "AUTHORPOG" (
    "F_AUTHORPOG_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "F_PATPROGRAM_ID" integer NOT NULL REFERENCES "PATPROGRAM" ("F_PATPROGRAM_ID"),
    "F_AUTHORPOG_NAME" varchar(255) NOT NULL,
    "F_AUTHORPOG_ORD" integer,
    "F_AUTHORPOG_USER" integer
);
CREATE TABLE "REPORT" (
    "F_REPORT_ID" integer NOT NULL PRIMARY KEY,
    "F_REPORT_NAME" varchar(255) NOT NULL,
    "F_REPORT_NUMBER" varchar(255) NOT NULL,
    "F_REPORT_DATE" date NOT NULL,
    "F_ORGANIZATION_ID" integer NOT NULL,
    "F_DEPARTMENT_ID" integer,
    "F_REPORT_PAGE" integer,
    "F_REPORT_ABSTRACT" text NOT NULL,
    "F_REPORT_XML" text NOT NULL,
    "F_REPORT_USER" integer
);
CREATE TABLE "AUTHORREP" (
    "F_AUTHORREP_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "F_REPORT_ID" integer NOT NULL REFERENCES "REPORT" ("F_REPORT_ID"),
    "F_AUTHORREP_NAME" varchar(255) NOT NULL,
    "F_AUTHORREP_ORD" integer,
    "F_AUTHORREP_USER" integer
);
CREATE TABLE "INVEST" (
    "F_INVEST_ID" integer NOT NULL PRIMARY KEY,
    "F_INVEST_NAME" varchar(255) NOT NULL,
    "F_INVEST_SUM" real NOT NULL,
    "F_INVEST_YEAR" integer NOT NULL,
    "F_ORGANIZATION_ID" integer,
    "F_PROJECT_ID" integer,
    "F_INVEST_SOURCE" varchar(255) NOT NULL,
    "F_INVEST_ABSTRACT" text NOT NULL,
    "F_INVEST_CLOB" text NOT NULL,
    "F_INVEST_USER" integer
);
CREATE TABLE "INVESTMAN" (
    "F_INVESTMAN_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "F_INVEST_ID" integer NOT NULL REFERENCES "INVEST" ("F_INVEST_ID"),
    "F_INVESTMAN_USER" integer
);
CREATE TABLE "COURSE" (
    "F_COURSE_ID" integer NOT NULL PRIMARY KEY,
    "F_COURSE_NAME" varchar(255) NOT NULL,
    "F_COURSE_YEAR" integer NOT NULL,
    "F_ORGANIZATION_ID" integer,
    "F_COURSE_ABSTRACT" text NOT NULL,
    "F_COURSE_XML" text NOT NULL,
    "F_COURSE_USER" integer
);
CREATE TABLE "AUTHORC" (
    "F_AUTHORC_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "F_COURSE_ID" integer NOT NULL REFERENCES "COURSE" ("F_COURSE_ID"),
    "F_AUTHORC_USER" integer
);
CREATE TABLE "DIPLOM" (
    "F_DIPLOM_ID" integer NOT NULL PRIMARY KEY,
    "F_DIPLOM_NAME" varchar(255) NOT NULL,
    "F_DIPLOM_AUTHOR" varchar(255) NOT NULL,
    "F_DIPLOM_YEAR" integer,
    "F_ORGANIZATION_ID" integer,
    "F_DIPLOM_ABSTRACT" text NOT NULL,
    "F_DIPLOM_XML" text NOT NULL,
    "F_DIPLOM_USER" integer
);
CREATE TABLE "DIPLOMHEAD" (
    "F_DIPLOMHEAD_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "F_DIPLOM_ID" integer NOT NULL REFERENCES "DIPLOM" ("F_DIPLOM_ID"),
    "F_DIPLOMHEAD_USER" integer
);
CREATE TABLE "BOARD" (
    "F_BOARD_ID" integer NOT NULL PRIMARY KEY,
    "F_BOARD_NUM" varchar(255) NOT NULL UNIQUE,
    "F_BOARD_NAME" varchar(255) NOT NULL,
    "F_ORGANIZATION_ID" integer,
    "F_BOARD_XML" text NOT NULL,
    "F_BOARD_USER" integer
);
CREATE TABLE "INBOARD" (
    "F_INBOARD_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "F_BOARD_ID" integer NOT NULL REFERENCES "BOARD" ("F_BOARD_ID"),
    "F_INBOARD_BEGIN" date NOT NULL,
    "F_INBOARD_END" date,
    "F_INBOARD_USER" integer
);
CREATE TABLE "DISSER" (
    "F_DISSER_ID" integer NOT NULL PRIMARY KEY,
    "F_DISSER_NAME" varchar(255) NOT NULL,
    "F_MAN_ID" integer REFERENCES "MAN" ("F_MAN_ID"),
    "F_BOARD_ID" integer NOT NULL REFERENCES "BOARD" ("F_BOARD_ID"),
    "F_BRANCH_ID" integer NOT NULL,
    "F_DISSER_BRANCHNUM" varchar(255) NOT NULL,
    "F_DISER_PRIOR" varchar(1) NOT NULL,
    "F_DISSER_YEAR" integer NOT NULL,
    "F_DISER_ABSTRACT" text NOT NULL,
    "F_DISSER_XML" text NOT NULL,
    "F_DISER_USER" integer
);
CREATE TABLE "DISERHEAD" (
    "F_DISERHEAD_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "F_DISSER_ID" integer NOT NULL REFERENCES "DISSER" ("F_DISSER_ID")
);
CREATE TABLE "BRANCH" (
    "F_BRANCH_ID" integer NOT NULL PRIMARY KEY,
    "F_BRANCH_NAME" varchar(255) NOT NULL
);
CREATE TABLE "PROJECT" (
    "F_PROJECT_ID" integer NOT NULL PRIMARY KEY,
    "F_PROJECT_NAME" varchar(255) NOT NULL,
    "F_ORGANIZATION_ID" integer,
    "F_PROJECT_BEGIN" date,
    "F_PROJECT_END" date,
    "F_PROJECT_ABSTRACT" text NOT NULL,
    "F_PROJECT_XML" text NOT NULL,
    "F_PROJECT_USER" integer
);
CREATE TABLE "PROJECTMAN" (
    "F_PROJECTMAN_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "F_PROJECT_ID" integer NOT NULL REFERENCES "PROJECT" ("F_PROJECT_ID"),
    "F_PROJECTMAN_BEGIN" date,
    "F_PROJECTMAN_END" date,
    "F_PROJECTMAN_USER" integer
);
CREATE TABLE "T_CONFS" (
    "CONF_ID" integer NOT NULL PRIMARY KEY,
    "NAME" varchar(255) NOT NULL,
    "CONF_PLACE" varchar(1000) NOT NULL,
    "YEAR" integer,
    "CONF_ABSTRACT" text NOT NULL,
    "CONF_XML" text NOT NULL,
    "CONF_USER" integer
);
CREATE TABLE "COMMITTEE" (
    "F_COMMITTEE_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "CONF_ID" integer NOT NULL REFERENCES "T_CONFS" ("CONF_ID"),
    "F_COMMITTEE_TYPE" integer NOT NULL,
    "F_COMMITTEE_USER" integer
);
CREATE TABLE "PRESENTATION" (
    "F_PRESENTATION_ID" integer NOT NULL PRIMARY KEY,
    "F_PRESENTATION_NAME" varchar(255) NOT NULL,
    "CONF_ID" integer NOT NULL REFERENCES "T_CONFS" ("CONF_ID"),
    "F_PRESENTATION_ABSTRACT" text NOT NULL,
    "F_PRESENTATION_XML" text NOT NULL,
    "F_PRESENTATION_USER" integer
);
CREATE TABLE "AUTHORD" (
    "F_AUTHORD_ID" integer NOT NULL PRIMARY KEY,
    "F_MAN_ID" integer NOT NULL REFERENCES "MAN" ("F_MAN_ID"),
    "F_PRESENTATION_ID" integer NOT NULL REFERENCES "PRESENTATION" ("F_PRESENTATION_ID"),
    "F_AUTHORD_USER" integer
);
CREATE TABLE "actstream_follow" (
    "id" integer NOT NULL PRIMARY KEY,
    "user_id" integer NOT NULL,
    "content_type_id" integer NOT NULL,
    "object_id" integer unsigned NOT NULL,
    UNIQUE ("user_id", "content_type_id", "object_id")
);
CREATE TABLE "actstream_action" (
    "id" integer NOT NULL PRIMARY KEY,
    "actor_content_type_id" integer NOT NULL,
    "actor_object_id" integer unsigned NOT NULL,
    "verb" varchar(255) NOT NULL,
    "description" text,
    "target_content_type_id" integer,
    "target_object_id" integer unsigned,
    "action_object_content_type_id" integer,
    "action_object_object_id" integer unsigned,
    "TIME_CREATED" datetime NOT NULL,
    "public" bool NOT NULL
);
CREATE TABLE "feedback_feedback" (
    "id" integer NOT NULL PRIMARY KEY,
    "user_id" integer,
    "message" text NOT NULL,
    "page" varchar(200) NOT NULL,
    "date" datetime NOT NULL,
    "seen" bool NOT NULL,
    "fixed" bool NOT NULL
);
CREATE TABLE "indexer_index" (
    "id" integer NOT NULL PRIMARY KEY,
    "app_label" varchar(32) NOT NULL,
    "module_name" varchar(32) NOT NULL,
    "column" varchar(32) NOT NULL,
    "value" varchar(128) NOT NULL,
    "object_id" integer unsigned NOT NULL,
    UNIQUE ("app_label", "module_name", "column", "value", "object_id")
);
CREATE TABLE "sentry_groupedmessage" (
    "id" integer NOT NULL PRIMARY KEY,
    "logger" varchar(64) NOT NULL,
    "class_name" varchar(128),
    "level" integer unsigned NOT NULL,
    "message" text NOT NULL,
    "traceback" text,
    "view" varchar(200),
    "checksum" varchar(32) NOT NULL,
    "data" text,
    "status" integer unsigned NOT NULL,
    "times_seen" integer unsigned NOT NULL,
    "last_seen" datetime NOT NULL,
    "first_seen" datetime NOT NULL,
    UNIQUE ("logger", "view", "checksum")
);
CREATE TABLE "sentry_message" (
    "id" integer NOT NULL PRIMARY KEY,
    "logger" varchar(64) NOT NULL,
    "class_name" varchar(128),
    "level" integer unsigned NOT NULL,
    "message" text NOT NULL,
    "traceback" text,
    "view" varchar(200),
    "checksum" varchar(32) NOT NULL,
    "data" text,
    "message_id" varchar(32) UNIQUE,
    "group_id" integer REFERENCES "sentry_groupedmessage" ("id"),
    "datetime" datetime NOT NULL,
    "url" varchar(200),
    "server_name" varchar(128) NOT NULL,
    "site" varchar(128)
);
CREATE TABLE "sentry_filtervalue" (
    "id" integer NOT NULL PRIMARY KEY,
    "key" varchar(32) NOT NULL,
    "value" varchar(200) NOT NULL,
    UNIQUE ("key", "value")
);
CREATE TABLE "userprofile_avatar" (
    "id" integer NOT NULL PRIMARY KEY,
    "image" varchar(100) NOT NULL,
    "user_id" integer NOT NULL,
    "date" datetime NOT NULL,
    "valid" bool NOT NULL,
    UNIQUE ("user_id", "valid")
);
CREATE TABLE "userprofile_emailvalidation" (
    "id" integer NOT NULL PRIMARY KEY,
    "user_id" integer NOT NULL UNIQUE,
    "email" varchar(75) NOT NULL,
    "key" varchar(70) NOT NULL UNIQUE,
    "verified" bool NOT NULL,
    "created" datetime NOT NULL
);
CREATE TABLE "object_permissions_group_perms" (
    "id" integer NOT NULL PRIMARY KEY,
    "user_id" integer,
    "group_id" integer,
    "obj_id" integer NOT NULL,
    "admin" integer NOT NULL
);
CREATE TABLE "object_permissions_organization_perms" (
    "id" integer NOT NULL PRIMARY KEY,
    "user_id" integer,
    "group_id" integer,
    "obj_id" integer NOT NULL,
    "view_stats" integer NOT NULL
);
CREATE TABLE "object_permissions_department_perms" (
    "id" integer NOT NULL PRIMARY KEY,
    "user_id" integer,
    "group_id" integer,
    "obj_id" integer NOT NULL,
    "view_stats" integer NOT NULL
);
CREATE TABLE "django_admin_log" (
    "id" integer NOT NULL PRIMARY KEY,
    "action_time" datetime NOT NULL,
    "user_id" integer NOT NULL,
    "content_type_id" integer,
    "object_id" text,
    "object_repr" varchar(200) NOT NULL,
    "action_flag" smallint unsigned NOT NULL,
    "change_message" text NOT NULL
);
CREATE TABLE "auth_permission" (
    "id" integer NOT NULL PRIMARY KEY,
    "name" varchar(50) NOT NULL,
    "content_type_id" integer NOT NULL,
    "codename" varchar(100) NOT NULL,
    UNIQUE ("content_type_id", "codename")
);
CREATE TABLE "auth_group_permissions" (
    "id" integer NOT NULL PRIMARY KEY,
    "group_id" integer NOT NULL,
    "permission_id" integer NOT NULL REFERENCES "auth_permission" ("id"),
    UNIQUE ("group_id", "permission_id")
);
CREATE TABLE "auth_group" (
    "id" integer NOT NULL PRIMARY KEY,
    "name" varchar(80) NOT NULL UNIQUE
);
CREATE TABLE "auth_user_user_permissions" (
    "id" integer NOT NULL PRIMARY KEY,
    "user_id" integer NOT NULL,
    "permission_id" integer NOT NULL REFERENCES "auth_permission" ("id"),
    UNIQUE ("user_id", "permission_id")
);
CREATE TABLE "auth_user_groups" (
    "id" integer NOT NULL PRIMARY KEY,
    "user_id" integer NOT NULL,
    "group_id" integer NOT NULL REFERENCES "auth_group" ("id"),
    UNIQUE ("user_id", "group_id")
);
CREATE TABLE "auth_user" (
    "id" integer NOT NULL PRIMARY KEY,
    "username" varchar(30) NOT NULL UNIQUE,
    "first_name" varchar(30) NOT NULL,
    "last_name" varchar(30) NOT NULL,
    "email" varchar(75) NOT NULL,
    "password" varchar(128) NOT NULL,
    "is_staff" bool NOT NULL,
    "is_active" bool NOT NULL,
    "is_superuser" bool NOT NULL,
    "last_login" datetime NOT NULL,
    "date_joined" datetime NOT NULL
);
CREATE TABLE "django_comments" (
    "id" integer NOT NULL PRIMARY KEY,
    "content_type_id" integer NOT NULL,
    "object_pk" text NOT NULL,
    "site_id" integer NOT NULL,
    "user_id" integer REFERENCES "auth_user" ("id"),
    "user_name" varchar(50) NOT NULL,
    "user_email" varchar(75) NOT NULL,
    "user_url" varchar(200) NOT NULL,
    "comment" text NOT NULL,
    "submit_date" datetime NOT NULL,
    "ip_address" char(15),
    "is_public" bool NOT NULL,
    "is_removed" bool NOT NULL
);
CREATE TABLE "django_comment_flags" (
    "id" integer NOT NULL PRIMARY KEY,
    "user_id" integer NOT NULL REFERENCES "auth_user" ("id"),
    "comment_id" integer NOT NULL REFERENCES "django_comments" ("id"),
    "flag" varchar(30) NOT NULL,
    "flag_date" datetime NOT NULL,
    UNIQUE ("user_id", "comment_id", "flag")
);
CREATE TABLE "django_content_type" (
    "id" integer NOT NULL PRIMARY KEY,
    "name" varchar(100) NOT NULL,
    "app_label" varchar(100) NOT NULL,
    "model" varchar(100) NOT NULL,
    UNIQUE ("app_label", "model")
);
CREATE TABLE "django_flatpage_sites" (
    "id" integer NOT NULL PRIMARY KEY,
    "flatpage_id" integer NOT NULL,
    "site_id" integer NOT NULL,
    UNIQUE ("flatpage_id", "site_id")
);
CREATE TABLE "django_flatpage" (
    "id" integer NOT NULL PRIMARY KEY,
    "url" varchar(100) NOT NULL,
    "title" varchar(200) NOT NULL,
    "content" text NOT NULL,
    "enable_comments" bool NOT NULL,
    "template_name" varchar(70) NOT NULL,
    "registration_required" bool NOT NULL
);
CREATE TABLE "django_session" (
    "session_key" varchar(40) NOT NULL PRIMARY KEY,
    "session_data" text NOT NULL,
    "expire_date" datetime NOT NULL
);
CREATE TABLE "django_site" (
    "id" integer NOT NULL PRIMARY KEY,
    "domain" varchar(100) NOT NULL,
    "name" varchar(50) NOT NULL
);
CREATE TABLE "ORGANIZATION" (
    "F_ORGANIZATION_ID" integer NOT NULL PRIMARY KEY,
    "F_ORGANIZATION_NAME" varchar(255) NOT NULL UNIQUE,
    "F_ORGANIZATION_USER" integer REFERENCES "auth_user" ("id")
);
CREATE TABLE "DEPARTMENT" (
    "F_DEPARTMENT_ID" integer NOT NULL PRIMARY KEY,
    "F_DEPARTMENT_NAME" varchar(255) NOT NULL,
    "F_ORGANIZATION_ID" integer NOT NULL REFERENCES "ORGANIZATION" ("F_ORGANIZATION_ID"),
    "F_DEPARTMENT_USER" integer REFERENCES "auth_user" ("id"),
    UNIQUE ("F_DEPARTMENT_NAME", "F_ORGANIZATION_ID")
);
CREATE TABLE "DEPARTMENTLINK" (
    "F_DEPARTMENTLINK_ID" integer NOT NULL PRIMARY KEY,
    "DEP_F_DEPARTMENT_ID" integer NOT NULL REFERENCES "DEPARTMENT" ("F_DEPARTMENT_ID"),
    "F_DEPARTMENT_ID" integer NOT NULL REFERENCES "DEPARTMENT" ("F_DEPARTMENT_ID"),
    "F_DEPARTMENTLINK_BEGIN" date NOT NULL,
    "F_DEPARTMENTLINK_END" date,
    "F_DEPARTMENTLINK_USER" integer REFERENCES "auth_user" ("id"),
    UNIQUE ("DEP_F_DEPARTMENT_ID", "F_DEPARTMENT_ID", "F_DEPARTMENTLINK_BEGIN")
);
CREATE INDEX "COLLECTION_5a514a6c" ON "COLLECTION" ("F_SERIA_ID");
CREATE INDEX "COLLECTION_6e52a3ea" ON "COLLECTION" ("F_PUBLISHING_ID");
CREATE INDEX "COLLECTION_199caeb" ON "COLLECTION" ("F_COLLECTION_USER");
CREATE INDEX "CORRECTOR_2ab10fea" ON "CORRECTOR" ("F_MAN_ID");
CREATE INDEX "CORRECTOR_780a6e5b" ON "CORRECTOR" ("F_COLLECTION_ID");
CREATE INDEX "CORRECTOR_5fe66469" ON "CORRECTOR" ("F_CORRECTOR_USER");
CREATE INDEX "SERIA_3c543058" ON "SERIA" ("F_SERIA_USER");
CREATE INDEX "JOURNAL_6e52a3ea" ON "JOURNAL" ("F_PUBLISHING_ID");
CREATE INDEX "JOURNAL_2bff28dc" ON "JOURNAL" ("LANG_ID");
CREATE INDEX "JOURNAL_7f453c3d" ON "JOURNAL" ("F_JOURNAL_USER");
CREATE INDEX "JALIAS_7b9e5f2b" ON "JALIAS" ("F_JOURNAL_ID");
CREATE INDEX "JALIAS_58ef0428" ON "JALIAS" ("F_ALIAS_ID");
CREATE INDEX "JALIAS_112a14f9" ON "JALIAS" ("F_JALIAS_USER");
CREATE INDEX "REDJOURNAL_2ab10fea" ON "REDJOURNAL" ("F_MAN_ID");
CREATE INDEX "REDJOURNAL_7b9e5f2b" ON "REDJOURNAL" ("F_JOURNAL_ID");
CREATE INDEX "REDJOURNAL_5a4da2bb" ON "REDJOURNAL" ("F_REDJOURNAL_USER");
CREATE INDEX "PATENT_22fb433" ON "PATENT" ("F_PATENT_USER");
CREATE INDEX "AUTHORP_2ab10fea" ON "AUTHORP" ("F_MAN_ID");
CREATE INDEX "AUTHORP_5113b907" ON "AUTHORP" ("F_PATENT_ID");
CREATE INDEX "AUTHORP_234121fb" ON "AUTHORP" ("F_AUTHORP_USER");
CREATE INDEX "ARTICLE_7b9e5f2b" ON "ARTICLE" ("F_JOURNAL_ID");
CREATE INDEX "ARTICLE_780a6e5b" ON "ARTICLE" ("F_COLLECTION_ID");
CREATE INDEX "ARTICLE_2bff28dc" ON "ARTICLE" ("LANG_ID");
CREATE INDEX "ARTICLE_5aae1d78" ON "ARTICLE" ("F_ARTICLE_USER");
CREATE INDEX "AUTHORA_2ab10fea" ON "AUTHORA" ("F_MAN_ID");
CREATE INDEX "AUTHORA_5504554" ON "AUTHORA" ("F_ARTICLE_ID");
CREATE INDEX "AUTHORA_7622b59c" ON "AUTHORA" ("F_AUTHORA_USER");
CREATE INDEX "BOOK_6e52a3ea" ON "BOOK" ("F_PUBLISHING_ID");
CREATE INDEX "BOOK_2bff28dc" ON "BOOK" ("LANG_ID");
CREATE INDEX "BOOK_3c772d48" ON "BOOK" ("F_BOOK_USER");
CREATE INDEX "AUTHORB_2ab10fea" ON "AUTHORB" ("F_MAN_ID");
CREATE INDEX "AUTHORB_3114f9f8" ON "AUTHORB" ("F_BOOK_ID");
CREATE INDEX "AUTHORB_481e61d3" ON "AUTHORB" ("F_AUTHORB_USER");
CREATE INDEX "FILES_5504554" ON "FILES" ("F_ARTICLE_ID");
CREATE INDEX "FILES_3114f9f8" ON "FILES" ("F_BOOK_ID");
CREATE INDEX "FILES_41283c59" ON "FILES" ("F_FILES_USER");
CREATE INDEX "LOGPARS_17f61d3c" ON "LOGPARS" ("F_LOGPARS_USER");
CREATE INDEX "MAN_1a0395d6" ON "MAN" ("F_MAN_USER");
CREATE INDEX "MANALIAS_2ab10fea" ON "MANALIAS" ("F_MAN_ID");
CREATE INDEX "MANALIAS_18c45791" ON "MANALIAS" ("F_MANALIAS_USER");
CREATE INDEX "MANSPOST_2ab10fea" ON "MANSPOST" ("F_MAN_ID");
CREATE INDEX "MANSPOST_3027c171" ON "MANSPOST" ("F_DEPARTMENT_ID");
CREATE INDEX "MANSPOST_77468eca" ON "MANSPOST" ("F_MANSPOST_USER");
CREATE INDEX "PATPROGRAM_5725dd1c" ON "PATPROGRAM" ("F_PATPROGRAM_USER");
CREATE INDEX "AUTHORPOG_2ab10fea" ON "AUTHORPOG" ("F_MAN_ID");
CREATE INDEX "AUTHORPOG_43b1fb44" ON "AUTHORPOG" ("F_PATPROGRAM_ID");
CREATE INDEX "AUTHORPOG_7190740b" ON "AUTHORPOG" ("F_AUTHORPOG_USER");
CREATE INDEX "REPORT_4df3f5ba" ON "REPORT" ("F_ORGANIZATION_ID");
CREATE INDEX "REPORT_3027c171" ON "REPORT" ("F_DEPARTMENT_ID");
CREATE INDEX "REPORT_6b3334ff" ON "REPORT" ("F_REPORT_USER");
CREATE INDEX "AUTHORREP_2ab10fea" ON "AUTHORREP" ("F_MAN_ID");
CREATE INDEX "AUTHORREP_74650db3" ON "AUTHORREP" ("F_REPORT_ID");
CREATE INDEX "AUTHORREP_401b0106" ON "AUTHORREP" ("F_AUTHORREP_USER");
CREATE INDEX "INVEST_4df3f5ba" ON "INVEST" ("F_ORGANIZATION_ID");
CREATE INDEX "INVEST_28ff31bd" ON "INVEST" ("F_PROJECT_ID");
CREATE INDEX "INVEST_28f6220e" ON "INVEST" ("F_INVEST_USER");
CREATE INDEX "INVESTMAN_2ab10fea" ON "INVESTMAN" ("F_MAN_ID");
CREATE INDEX "INVESTMAN_2c0eaa8e" ON "INVESTMAN" ("F_INVEST_ID");
CREATE INDEX "INVESTMAN_3ddfa26f" ON "INVESTMAN" ("F_INVESTMAN_USER");
CREATE INDEX "COURSE_4df3f5ba" ON "COURSE" ("F_ORGANIZATION_ID");
CREATE INDEX "COURSE_79146c7a" ON "COURSE" ("F_COURSE_USER");
CREATE INDEX "AUTHORC_2ab10fea" ON "AUTHORC" ("F_MAN_ID");
CREATE INDEX "AUTHORC_1c069c52" ON "AUTHORC" ("F_COURSE_ID");
CREATE INDEX "AUTHORC_a7b107a" ON "AUTHORC" ("F_AUTHORC_USER");
CREATE INDEX "DIPLOM_4df3f5ba" ON "DIPLOM" ("F_ORGANIZATION_ID");
CREATE INDEX "DIPLOM_3fb295d8" ON "DIPLOM" ("F_DIPLOM_USER");
CREATE INDEX "DIPLOMHEAD_2ab10fea" ON "DIPLOMHEAD" ("F_MAN_ID");
CREATE INDEX "DIPLOMHEAD_2aa1b98" ON "DIPLOMHEAD" ("F_DIPLOM_ID");
CREATE INDEX "DIPLOMHEAD_7aa98112" ON "DIPLOMHEAD" ("F_DIPLOMHEAD_USER");
CREATE INDEX "BOARD_4df3f5ba" ON "BOARD" ("F_ORGANIZATION_ID");
CREATE INDEX "BOARD_6aff1f48" ON "BOARD" ("F_BOARD_USER");
CREATE INDEX "INBOARD_2ab10fea" ON "INBOARD" ("F_MAN_ID");
CREATE INDEX "INBOARD_10c3bddc" ON "INBOARD" ("F_BOARD_ID");
CREATE INDEX "INBOARD_3ee252e1" ON "INBOARD" ("F_INBOARD_USER");
CREATE INDEX "DISSER_2ab10fea" ON "DISSER" ("F_MAN_ID");
CREATE INDEX "DISSER_10c3bddc" ON "DISSER" ("F_BOARD_ID");
CREATE INDEX "DISSER_261a2279" ON "DISSER" ("F_BRANCH_ID");
CREATE INDEX "DISSER_31639763" ON "DISSER" ("F_DISER_USER");
CREATE INDEX "DISERHEAD_2ab10fea" ON "DISERHEAD" ("F_MAN_ID");
CREATE INDEX "DISERHEAD_7ea3b63d" ON "DISERHEAD" ("F_DISSER_ID");
CREATE INDEX "PROJECT_4df3f5ba" ON "PROJECT" ("F_ORGANIZATION_ID");
CREATE INDEX "PROJECT_4dd7681f" ON "PROJECT" ("F_PROJECT_USER");
CREATE INDEX "PROJECTMAN_2ab10fea" ON "PROJECTMAN" ("F_MAN_ID");
CREATE INDEX "PROJECTMAN_28ff31bd" ON "PROJECTMAN" ("F_PROJECT_ID");
CREATE INDEX "PROJECTMAN_474775e0" ON "PROJECTMAN" ("F_PROJECTMAN_USER");
CREATE INDEX "T_CONFS_426c58aa" ON "T_CONFS" ("CONF_USER");
CREATE INDEX "COMMITTEE_2ab10fea" ON "COMMITTEE" ("F_MAN_ID");
CREATE INDEX "COMMITTEE_59d9cbbe" ON "COMMITTEE" ("CONF_ID");
CREATE INDEX "COMMITTEE_12907a27" ON "COMMITTEE" ("F_COMMITTEE_USER");
CREATE INDEX "PRESENTATION_59d9cbbe" ON "PRESENTATION" ("CONF_ID");
CREATE INDEX "PRESENTATION_af0c557" ON "PRESENTATION" ("F_PRESENTATION_USER");
CREATE INDEX "AUTHORD_2ab10fea" ON "AUTHORD" ("F_MAN_ID");
CREATE INDEX "AUTHORD_7a9a78dd" ON "AUTHORD" ("F_PRESENTATION_ID");
CREATE INDEX "AUTHORD_432e56bf" ON "AUTHORD" ("F_AUTHORD_USER");
CREATE INDEX "actstream_follow_403f60f" ON "actstream_follow" ("user_id");
CREATE INDEX "actstream_follow_1bb8f392" ON "actstream_follow" ("content_type_id");
CREATE INDEX "actstream_action_331f64c6" ON "actstream_action" ("actor_content_type_id");
CREATE INDEX "actstream_action_567f53ff" ON "actstream_action" ("target_content_type_id");
CREATE INDEX "actstream_action_5270a1bb" ON "actstream_action" ("action_object_content_type_id");
CREATE INDEX "feedback_feedback_403f60f" ON "feedback_feedback" ("user_id");
CREATE INDEX "sentry_groupedmessage_147bb5bf" ON "sentry_groupedmessage" ("logger");
CREATE INDEX "sentry_groupedmessage_5420e8b5" ON "sentry_groupedmessage" ("class_name");
CREATE INDEX "sentry_groupedmessage_2a8f42e8" ON "sentry_groupedmessage" ("level");
CREATE INDEX "sentry_groupedmessage_36528e23" ON "sentry_groupedmessage" ("status");
CREATE INDEX "sentry_groupedmessage_ee4a0d0" ON "sentry_groupedmessage" ("last_seen");
CREATE INDEX "sentry_groupedmessage_4d23e6a1" ON "sentry_groupedmessage" ("first_seen");
CREATE INDEX "sentry_message_147bb5bf" ON "sentry_message" ("logger");
CREATE INDEX "sentry_message_5420e8b5" ON "sentry_message" ("class_name");
CREATE INDEX "sentry_message_2a8f42e8" ON "sentry_message" ("level");
CREATE INDEX "sentry_message_425ae3c4" ON "sentry_message" ("group_id");
CREATE INDEX "sentry_message_882a7e0" ON "sentry_message" ("datetime");
CREATE INDEX "sentry_message_326e5a6b" ON "sentry_message" ("server_name");
CREATE INDEX "sentry_message_1ff577e6" ON "sentry_message" ("site");
CREATE INDEX "userprofile_avatar_403f60f" ON "userprofile_avatar" ("user_id");
CREATE INDEX "object_permissions_group_perms_403f60f" ON "object_permissions_group_perms" ("user_id");
CREATE INDEX "object_permissions_group_perms_425ae3c4" ON "object_permissions_group_perms" ("group_id");
CREATE INDEX "object_permissions_group_perms_1667bb98" ON "object_permissions_group_perms" ("obj_id");
CREATE INDEX "object_permissions_organization_perms_403f60f" ON "object_permissions_organization_perms" ("user_id");
CREATE INDEX "object_permissions_organization_perms_425ae3c4" ON "object_permissions_organization_perms" ("group_id");
CREATE INDEX "object_permissions_organization_perms_1667bb98" ON "object_permissions_organization_perms" ("obj_id");
CREATE INDEX "object_permissions_department_perms_403f60f" ON "object_permissions_department_perms" ("user_id");
CREATE INDEX "object_permissions_department_perms_425ae3c4" ON "object_permissions_department_perms" ("group_id");
CREATE INDEX "object_permissions_department_perms_1667bb98" ON "object_permissions_department_perms" ("obj_id");
CREATE INDEX "django_admin_log_403f60f" ON "django_admin_log" ("user_id");
CREATE INDEX "django_admin_log_1bb8f392" ON "django_admin_log" ("content_type_id");
CREATE INDEX "auth_permission_1bb8f392" ON "auth_permission" ("content_type_id");
CREATE INDEX "auth_group_permissions_425ae3c4" ON "auth_group_permissions" ("group_id");
CREATE INDEX "auth_group_permissions_1e014c8f" ON "auth_group_permissions" ("permission_id");
CREATE INDEX "auth_user_user_permissions_403f60f" ON "auth_user_user_permissions" ("user_id");
CREATE INDEX "auth_user_user_permissions_1e014c8f" ON "auth_user_user_permissions" ("permission_id");
CREATE INDEX "auth_user_groups_403f60f" ON "auth_user_groups" ("user_id");
CREATE INDEX "auth_user_groups_425ae3c4" ON "auth_user_groups" ("group_id");
CREATE INDEX "django_comments_1bb8f392" ON "django_comments" ("content_type_id");
CREATE INDEX "django_comments_6223029" ON "django_comments" ("site_id");
CREATE INDEX "django_comments_403f60f" ON "django_comments" ("user_id");
CREATE INDEX "django_comment_flags_403f60f" ON "django_comment_flags" ("user_id");
CREATE INDEX "django_comment_flags_64c238ac" ON "django_comment_flags" ("comment_id");
CREATE INDEX "django_comment_flags_111c90f9" ON "django_comment_flags" ("flag");
CREATE INDEX "django_flatpage_sites_21210108" ON "django_flatpage_sites" ("flatpage_id");
CREATE INDEX "django_flatpage_sites_6223029" ON "django_flatpage_sites" ("site_id");
CREATE INDEX "django_flatpage_a4b49ab" ON "django_flatpage" ("url");
CREATE INDEX "django_session_3da3d3d8" ON "django_session" ("expire_date");
CREATE INDEX "ORGANIZATION_21d44dc2" ON "ORGANIZATION" ("F_ORGANIZATION_USER");
CREATE INDEX "DEPARTMENT_4df3f5ba" ON "DEPARTMENT" ("F_ORGANIZATION_ID");
CREATE INDEX "DEPARTMENT_35dec97b" ON "DEPARTMENT" ("F_DEPARTMENT_USER");
CREATE INDEX "DEPARTMENTLINK_653fbde5" ON "DEPARTMENTLINK" ("DEP_F_DEPARTMENT_ID");
CREATE INDEX "DEPARTMENTLINK_3027c171" ON "DEPARTMENTLINK" ("F_DEPARTMENT_ID");
CREATE INDEX "DEPARTMENTLINK_637f9fdb" ON "DEPARTMENTLINK" ("F_DEPARTMENTLINK_USER");
