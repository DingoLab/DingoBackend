--------------------------------------------------------------------------------
-- Dindo Database
-- PostgreSQL
-- PL/pgSQL
-- (C) DindoLab
-- sql/dindo.sql
--------------------------------------------------------------------------------



-- 创建数据库
CREATE DATABASE dingo
  WITH OWNER = postgres
       ENCODING = 'UTF8'
       TABLESPACE = pg_default
       LC_COLLATE = 'en_US.utf8'
       LC_CTYPE = 'en_US.utf8'
       CONNECTION LIMIT = -1
;
COMMENT ON DATABASE dingo
  IS '软件工程大作业－Dingo 后端－Dindo 的数据库'
;



-- 操作记录
CREATE SCHEMA opt_log; -- 创建
GRANT ALL
  ON SCHEMA opt_log
  TO postgres
; -- 权限
COMMENT ON SCHEMA opt_log
  IS '用于记录各类操作的模式'
;

-- 创建表
-- 账户表
CREATE TABLE table_account
  ( key_uid  VARCHAR(64) PRIMARY KEY
  , key_name VARCHAR(64) UNIQUE
  , key_tel  BIGINT         UNIQUE NOT NULL CHECK(key_tel > 10000000000 AND key_tel < 20000000000)
  , key_pash VARCHAR(64) NOT NULL CHECK(char_length(key_pash) = 64)
  )
;
COMMENT ON TABLE table_account
  IS '账户的信息的表'
;
-- 用户信息表
CREATE TABLE table_usr
  ( key_uid     VARCHAR(64) PRIMARY KEY REFERENCES table_account(key_uid)
  , key_email   TEXT        NOT NULL
  , key_rname   VARCHAR(64) NOT NULL
  , key_prcid   VARCHAR(18) NOT NULL CHECK(char_length(key_prcid) = 18)
  , key_addr    TEXT        NOT NULL
  , key_status  VARCHAR(2)  NOT NULL CHECK(ARRAY[ascii(key_status)] <@ ARRAY[78,80]) --  待审核，通过
  )
;
COMMENT ON TABLE table_usr
  IS '用户的表，主要记录认证信息'
;
-- 账户图片表
CREATE TABLE table_apic
  ( key_pid    VARCHAR(64) PRIMARY KEY
  , key_uid    VARCHAR(64) NOT NULL REFERENCES table_account(key_uid)
  , binary_pic BYTEA       NOT NULL
  , key_type   INT         DEFAULT 0
  )
;
COMMENT ON TABLE table_apic
  IS '账户的相关照片'
;
-- 收货地址
CREATE TABLE table_addr
  ( key_aid  CHAR(64) NOT NULL PRIMARY KEY
  , key_uid  CHAR(64) NOT NULL REFERENCES table_account(key_uid)
  , key_addr TEXT     NOT NULL
  , key_zip  INT      NOT NULL CHECK(key_zip <1000000 AND key_zip > 99999)
  )
;
COMMENT ON TABLE table_addr
  IS '收货地址'
;
-- 任务表
CREATE TABLE table_task
  ( key_tid VARCHAR(64) PRIMARY KEY
  , key_ca  VARCHAR(64) REFERENCES table_account(key_uid)
  , key_cb  VARCHAR(64) REFERENCES table_account(key_uid) CHECK (key_ca <> key_cb)
  )
;
COMMENT ON TABLE table_task
  IS '代收任务纪录'
;
-- 任务信息表
CREATE TABLE table_task_info
  ( key_tid  VARCHAR(64) PRIMARY KEY REFERENCES table_task(key_tid)
  , key_ew   REAL        NOT NULL
  , key_ns   REAL        NOT NULL
  , key_r    REAL        NOT NULL
  , key_w    REAL        NOT NULL
  , key_size REAL[]      NOT NULL CHECK(array_length(key_size,1) = 3)
  , key_note TEXT
  , key_cost INT         NOT NULL
  , key_des  TEXT
  )
;
COMMENT ON TABLE table_task_info
  IS '任务信息的表'
;
-- 任务价格表
CREATE TABLE table_task_cost
  ( key_tid VARCHAR(64) NOT NULL PRIMARY KEY REFERENCES table_task(key_tid)
  , key_ad  INT[]       NOT NULL DEFAULT ARRAY[0]
  , key_bd  INT[]       NOT NULL DEFAULT ARRAY[0]
  )
;
COMMENT ON TABLE table_task_cost
  IS '任务中的价格'
;
-- 可代收任务表
CREATE TABLE table_dd
  ( key_did VARCHAR(64) NOT NULL PRIMARY KEY
  , key_dd  TEXT        NOT NULL
  , key_ew  REAL        NOT NULL
  , key_ns  REAL        NOT NULL
  , key_r   REAL        NOT NULL
  )
;
COMMENT ON TABLE table_dd
  IS '可代收任务的表'
;
-- TmpToken
CREATE TABLE table_tmptoken
  ( key_tmptoken VARCHAR(150)             NOT NULL PRIMARY KEY
  , key_timeup   TIMESTAMP WITH TIME ZONE NOT NULL
  , key_uid      VARCHAR(64)              NOT NULL REFERENCES table_account(key_uid)
  )
;
COMMENT ON TABLE table_tmptoken
  IS '临时 Token 的表'
;
-- 临时纪录的表
CREATE TABLE opt_log.log_login
  ( update_time TIMESTAMP WITH TIME ZONE NOT NULL
  , uid         VARCHAR(64)              NOT NULL
  , db_usr      VARCHAR(64)              NOT NULL
  , token       VARCHAR(150)             NOT NULL
  )
;
CREATE TABLE opt_log.log_data_change
  ( update_time   TIMESTAMP WITH TIME ZONE NOT NULL
  , db_usr        VARCHAR(64)              NOT NULL
  , trigger_anme  VARCHAR(64)              NOT NULL
  , trigger_when  VARCHAR(8)               NOT NULL
  , trigger_level VARCHAR(16)              NOT NULL
  , trigger_opt   VARCHAR(16)              NOT NULL
  , trigger_relid OID                      NOT NULL
  , trigger_tname VARCHAR(64)              NOT NULL
  , trigger_sname VARCHAR(64)              NOT NULL
  , tg_argv       TEXT[]
  , old_val       JSON
  , new_val       JSON
  )
;


-- 触发器
-- 登陆触发器
-- 触发器函数
CREATE FUNCTION log_login_trigger ()
RETURNS trigger AS
$$
BEGIN
  INSERT INTO opt_log.log_login(update_time,uid,db_usr,token)
    VALUES(clock_timestamp(),NEW.key_uid,USER,NEW.key_tmptoken);
  RETURN NULL;
END;
$$
LANGUAGE "plpgsql";
-- 触发器
CREATE TRIGGER log_login_trigger
  BEFORE INSERT ON table_tmptoken
  FOR ROW EXECUTE PROCEDURE log_login_trigger()
;
-- 限制提交时就确认用户认证
-- 触发器函数
CREATE FUNCTION add_usr_trigger ()
RETURNS trigger AS
$add_usr_trigger$
BEGIN
  NEW.key_status := 'N';
  RETURN NEW;
END;
$add_usr_trigger$
LANGUAGE "plpgsql";
-- 触发器
CREATE TRIGGER add_usr_trigger
  BEFORE INSERT ON table_usr
  FOR EACH ROW EXECUTE PROCEDURE add_usr_trigger()
;

-- 表数据修改纪录
-- 触发器函数
CREATE FUNCTION data_changing ()
RETURNS trigger AS
$$
BEGIN
  IF TG_OP = 'INSERT' THEN
    INSERT INTO opt_log.log_data_change(update_time,db_usr,trigger_anme,trigger_when,trigger_level,trigger_opt,trigger_relid,trigger_tname,trigger_sname,tg_argv,new_val)
      VALUES(clock_timestamp(),USER,TG_NAME,TG_WHEN,TG_LEVEL,TG_OP,TG_RELID,TG_TABLE_NAME,TG_TABLE_SCHEMA,TG_ARGV,row_to_json(NEW))
    ;
    RETURN NEW;
  END IF;
  IF TG_OP = 'UPDATE' THEN
    INSERT INTO opt_log.log_data_change(update_time,db_usr,trigger_anme,trigger_when,trigger_level,trigger_opt,trigger_relid,trigger_tname,trigger_sname,tg_argv,old_val,new_val)
      VALUES(clock_timestamp(),USER,TG_NAME,TG_WHEN,TG_LEVEL,TG_OP,TG_RELID,TG_TABLE_NAME,TG_TABLE_SCHEMA,TG_ARGV,row_to_json(OLD),row_to_json(NEW))
    ;
    RETURN NEW;
  END IF;
  IF TG_OP = 'DELETE' THEN
    INSERT INTO opt_log.log_data_change(update_time,db_usr,trigger_anme,trigger_when,trigger_level,trigger_opt,trigger_relid,trigger_tname,trigger_sname,tg_argv,old_val)
      VALUES(clock_timestamp(),USER,TG_NAME,TG_WHEN,TG_LEVEL,TG_OP,TG_RELID,TG_TABLE_NAME,TG_TABLE_SCHEMA,TG_ARGV,row_to_json(OLD))
    ;
    RETURN OLD;
  END IF;
  RETURN NULL;
END;
$$
LANGUAGE "plpgsql";
-- 触发器
CREATE TRIGGER data_changing
  BEFORE INSERT OR UPDATE OR DELETE  ON table_addr
  FOR EACH ROW EXECUTE PROCEDURE data_changing()
;
CREATE TRIGGER data_changing
  BEFORE INSERT OR UPDATE OR DELETE  ON table_apic
  FOR EACH ROW EXECUTE PROCEDURE data_changing()
;
CREATE TRIGGER data_changing
  BEFORE INSERT OR UPDATE OR DELETE  ON table_account
  FOR EACH ROW EXECUTE PROCEDURE data_changing()
;
CREATE TRIGGER data_changing
  BEFORE INSERT OR UPDATE OR DELETE  ON table_usr
  FOR EACH ROW EXECUTE PROCEDURE data_changing()
;
CREATE TRIGGER data_changing
  BEFORE INSERT OR UPDATE OR DELETE  ON table_task
  FOR EACH ROW EXECUTE PROCEDURE data_changing()
;
CREATE TRIGGER data_changing
  BEFORE INSERT OR UPDATE OR DELETE  ON table_task_cost
  FOR EACH ROW EXECUTE PROCEDURE data_changing()
;
CREATE TRIGGER data_changing
  BEFORE INSERT OR UPDATE OR DELETE  ON table_tmptoken
  FOR EACH ROW EXECUTE PROCEDURE data_changing()
;
CREATE TRIGGER data_changing
  BEFORE INSERT OR UPDATE OR DELETE  ON table_task_info
  FOR EACH ROW EXECUTE PROCEDURE data_changing()
;
CREATE TRIGGER data_changing
  BEFORE INSERT OR UPDATE OR DELETE  ON table_dd
  FOR EACH ROW EXECUTE PROCEDURE data_changing()
;

-- 用户
-- 用户认证管理员账号
CREATE USER umchecker
  NOSUPERUSER
  NOCREAREDB
  NOCREATEROLE
  NOCREATEUSER
  LOGIN
  ENCRYPTED PASSWORD 'umchecker'
  CONNECTION LIMIT 10
;
GRANT SELECT UPDATE
  ON table_usr
  TO umchecker
;
